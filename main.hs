-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Data.List
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, size, toList)
import Utils
import System.IO
import Data.Functor (void, (<&>))
import Type.Env
import Type.Sexp
import Type.CompileResult
import Text.Read (readMaybe)
import Type.Parser.String 
import Parsers.String
import Parsers.Sexp
import Type.CompileResult
import Distribution.Simple.Utils (safeLast, safeInit)

data Lit
    = BoolLit Bool
    | IntLit Int
    deriving (Show, Eq)

data Expr
    = CallExpr [Expr]
    | LitExpr Lit
    | VarExpr String
    deriving (Show, Eq)

data Body
    = FunBody String [(String, Sexp)] Sexp [Body]
    | RetBody (Maybe Expr)
    | VarBody [String] Sexp Expr 
    | IfBody [(Expr, [Body])] (Maybe [Body])
    | ForBody (Maybe Stat) (Maybe Expr) (Maybe Stat) [Body]
    | CallBody [Expr]
    deriving (Show, Eq)

data Stat
    = VarStat [String] Sexp Expr 
    | CallStat [Expr]
    deriving (Show, Eq)

data Top
    = FunTop String [(String, Sexp)] Sexp [Body]
    | VarTop [String] Sexp Expr 
    deriving Show

miscErr :: CompileResult a
miscErr = CompileResult $ Left [MiscError]

------------------------------------------------------

parseLit :: Sexp -> CompileResult Lit
parseLit (Atom "true") = pure $ BoolLit True
parseLit (Atom "false") = pure $ BoolLit False
parseLit (Atom numText) =
    if isNum $ head numText
    then
        if all isNum numText
        then pure $ IntLit (read numText :: Int)
        else miscErr
    else empty
parseLit (List _) = CompileResult $ Right Nothing

parseCall :: Sexp -> CompileResult [Expr]
parseCall (Atom _) = empty
parseCall (List []) = empty
parseCall (List exprs) = traverse parseExpr exprs

parseIdentifier :: Sexp -> CompileResult String
parseIdentifier (Atom a) = pure a
parseIdentifier _ = empty

parseExpr :: Sexp -> CompileResult Expr
parseExpr s = 
    (CallExpr <$> parseCall s) 
        <|> (LitExpr <$> parseLit s)
        <|> (VarExpr <$> parseIdentifier s)

------------------------------------------------------

parseFun :: Sexp -> CompileResult (String, [(String, Sexp)], Sexp, [Body])
parseFun (Atom _) = empty
parseFun (List ((Atom "fun") : (Atom name) : (List args) : rest))
    | odd $ length args = miscErr
    | any (\case (List _) -> True; (Atom _) -> False) $ fsts args = miscErr
    | hasArrow && (length rest == 1) = miscErr
    | otherwise = (name, argPairs, returnType, ) <$> statements
        where
            argPairs = (\l -> (fromAtom $ head l, l !! 1)) <$> chunk 2 args
            hasArrow = (rest !? 0) == Just (Atom "->")
            returnType = if hasArrow then rest !! 1 else Atom "Void"
            statementSexps = if hasArrow then drop 2 rest else rest
            statements = mapM parseBody statementSexps
parseFun (List (Atom "fun" : rest)) = miscErr
parseFun _ = empty

parseRet :: Sexp -> CompileResult (Maybe Expr)
parseRet (List [Atom "return", expr]) = Just <$> parseExpr expr
parseRet (List [Atom "return"]) = pure Nothing
parseRet _ = empty

parseVar :: Sexp -> CompileResult ([String], Sexp, Expr)
parseVar (Atom _) = empty
parseVar (List (Atom "var" : rest)) =
    let len = length rest in
    if len < 2 || len > 3 then miscErr else
    let 
        names = elseCompileError MiscError (flatNotEmptyAtoms $ head rest)
        third = fromMaybe (List [Atom "init"]) (rest !? 2)
        expr = parseExpr third
    in
        (, rest !! 1, ) <$> names <*> expr
parseVar _ = empty

parseIf :: Sexp -> CompileResult ([(Expr, [Body])], Maybe [Body])
parseIf (Atom _) = empty
parseIf (List [Atom "if"]) = miscErr
parseIf (List sexps@(Atom "if" : _)) = 
    let
        keywords = Atom <$> ["if", ":else-if", ":else"]
        ifSplits = splitAndKeepDelim (`elem` keywords) sexps
    in do
        ifChunks <- traverse parseIfChunk (traceLabel "ifSplits" ifSplits)
        if not (validateIfChunks ifChunks) then 
            miscErr
        else
            let (conds, elses) = break isElseChunk ifChunks
            in pure (fromCondChunk <$> conds, fromElseChunk <$> safeLast elses)
parseIf _ = empty

data IfChunk
    = If (Expr, [Body])
    | ElseIf (Expr, [Body])
    | Else [Body]
    deriving (Show, Eq)

isIfChunk :: IfChunk -> Bool
isIfChunk (If _) = True
isIfChunk _ = False

isElseIfChunk :: IfChunk -> Bool
isElseIfChunk (ElseIf _) = True
isElseIfChunk _ = False

isElseChunk :: IfChunk -> Bool
isElseChunk (Else _) = True
isElseChunk _ = False

fromCondChunk :: IfChunk -> (Expr, [Body])
fromCondChunk (If t) = t
fromCondChunk (ElseIf t) = t

fromElseChunk :: IfChunk -> [Body]
fromElseChunk (Else b) = b

validateIfChunks :: [IfChunk] -> Bool
validateIfChunks ((If _) : cs) = initIsGood && lastIsGood
    where 
        initIsGood = all isElseIfChunk (safeInit cs)
        lastIsGood = ((not . isIfChunk) <$> safeLast cs) /= Just False
validateIfChunks _ = False

parseIfChunk :: [Sexp] -> CompileResult IfChunk
parseIfChunk (Atom "if" : (s : ss)) = If <$> liftA2 (,) (parseExpr s) (traverse parseBody ss)
parseIfChunk (Atom ":else-if" : (s : ss)) = ElseIf <$> liftA2 (,) (parseExpr s) (traverse parseBody ss)
parseIfChunk (Atom ":else" : rest) = Else <$> traverse parseBody rest
parseIfChunk _ = miscErr

allowEmptyList :: (Sexp -> CompileResult a) -> (Sexp -> CompileResult (Maybe a))
allowEmptyList _ (List []) = pure Nothing
allowEmptyList f s = Just <$> f s

parseFor :: Sexp -> CompileResult (Maybe Stat, Maybe Expr, Maybe Stat, [Body])
parseFor (Atom _) = empty
parseFor (List (Atom "for" : rest)) = 
    if length rest < 3 then miscErr else
    let 
        init = (allowEmptyList parseStat) (rest !! 0)
        cond = (allowEmptyList parseExpr) (rest !! 1)
        update = (allowEmptyList parseStat) (rest !! 2)
        body = traverse parseBody (drop 3 rest)
    in 
        (,,,) <$> init <*> cond <*> update <*> body
parseFor _ = empty

parseBody :: Sexp -> CompileResult Body
parseBody s = 
    (uncurry4 FunBody <$> parseFun s)
    <|> (RetBody <$> parseRet s)
    <|> (uncurry3 VarBody <$> parseVar s)
    -- <|> (uncurry IfBody <$> parseIf s)
    <|> (uncurry4 ForBody <$> parseFor s)
    <|> (CallBody <$> parseCall s)

parseStat :: Sexp -> CompileResult Stat
parseStat s = 
    (uncurry3 VarStat <$> parseVar s)
    <|> (CallStat <$> parseCall s)

------------------------------------------------------

filePathSexps :: FilePath -> IO (Maybe [Sexp])
filePathSexps path = do
    text <- readFile path
    case runParser sexps text of
        Just (unparsed, sexps) -> pure (if unparsed == "" then Just sexps else Nothing)
        Nothing -> pure Nothing

greenFilesSexps :: IO (Maybe [Sexp])
greenFilesSexps = do
    paths <- findRelativeGreenFilePaths ""
    sexps <- traverse filePathSexps paths
    pure $ concat <$> sequence sexps

------------------------------------------------------

main :: IO ()
main = do
    sexps <- greenFilesSexps
    case sexps >>= globalEnv of
        Just env -> putStrLn $ show env
        Nothing -> putStrLn "Compilation Error"