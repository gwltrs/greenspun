{-# LANGUAGE LambdaCase #-}

module Parsers.Sexp where

import Type.CompileResult
import Type.Top
import Type.Sexp
import Type.Env
import Data.Set hiding (drop, empty, null)
import Utils
import Data.Functor
import Control.Applicative
import Distribution.Simple.Utils (safeLast, safeInit)
import Data.Maybe

parseFunDec :: Sexp -> Maybe [FunDec]
parseFunDec (Atom _) = Just []
parseFunDec (List ((Atom "fun") : (Atom name) : (List args) : rest))
    | odd $ length args = Nothing
    | any (\case (List _) -> True; (Atom _) -> False) $ fsts args = Nothing
    | hasArrow && (length rest == 1) = Nothing
    | otherwise = Just [FunDec { funName = name, funType = List ([Atom "->"] <> snds args <> [returnType]) }]
        where
            hasArrow = (rest !? 0) == Just (Atom "->")
            returnType = if hasArrow then rest !! 1 else Atom "Void"
-- parseFunDec (List ((Atom "fun") : (returnType : ((Atom name): (List args: _)))))
--     | odd $ length args = Nothing
--     | any (\case (List _) -> True; (Atom _) -> False) $ snds args = Nothing
--     | otherwise = Just $ [FunDec { funName = name, funType = List ([Atom "Fun", returnType] <> fsts args) }]
parseFunDec (List (Atom "fun" : rest)) = Nothing
parseFunDec _ = Just []

parseVarDec :: Sexp -> Maybe [VarDec]
parseVarDec (Atom _) = Just []
parseVarDec (List ((Atom "var") : rest)) =
    let len = length rest in
    if len < 2 || len > 3 then Nothing else
    let names = flatNotEmptyAtoms $ head rest in
    names <&> (\ns -> ns <&> (\n -> VarDec { varName = n, varType = rest !! 1 }))
parseVarDec _ = Just []

globalEnv :: [Sexp] -> Maybe Env
globalEnv ss = do
    funDecsList <- concat <$> mapM parseFunDec ss
    varDecsList <- concat <$> mapM parseVarDec ss
    let funDecsSet = fromList funDecsList
    let varDecsSet = fromList varDecsList
    let nameSet = fromList ((funName <$> toList funDecsSet) <> (varName <$> toList varDecsSet))
    if 
        length funDecsList == size funDecsSet &&
        length varDecsList == size varDecsSet &&
        length nameSet == length funDecsList + length varDecsList
    then
        Just $ Env { varDecs = varDecsSet, funDecs = funDecsSet }
    else
        Nothing

parseTop :: Sexp -> CompileResult Top
parseTop s = (uncurry4 FunTop <$> parseFun s)
    <|> (uncurry3 VarTop <$> parseVar s)

miscErr :: CompileResult a
miscErr = CompileResult $ Left [MiscError]

parseLit :: Sexp -> CompileResult Lit
parseLit (Atom "true") = pure $ BoolLit True
parseLit (Atom "false") = pure $ BoolLit False
parseLit (Atom numText) =
    if (isNum ||| (== '-')) $ head numText
    then
        if all isNum (drop 1 numText)
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

parseVar :: Sexp -> CompileResult ([String], Sexp, [Maybe Expr])
parseVar (Atom _) = empty
parseVar (List (Atom "var" : rest)) =
    let 
        finalizeExprs :: Int -> [Maybe Expr] -> CompileResult [Maybe Expr]
        finalizeExprs namesLen valueExprs_ =
            let valLen = length valueExprs_ in
            if valLen == 0 then
                pure $ replicate namesLen (Just $ CallExpr [VarExpr "init"])
            else if valLen == namesLen then
                pure valueExprs_
            else if valLen == 1 then
                pure $ replicate namesLen (head valueExprs_)
            else
                miscErr
    in do
        (names :: [String]) <- elseCompileError MiscError (flatNotEmptyAtoms $ head rest)
        (valueExprs :: [Maybe Expr]) <- traverse (allowNothing (== Atom "_") parseExpr) (drop 2 rest)
        (valueExprs2 :: [Maybe Expr]) <- finalizeExprs (length names) valueExprs
        pure (names, rest !! 1, valueExprs2)
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

allowNothing :: (Sexp -> Bool) -> (Sexp -> CompileResult a) -> (Sexp -> CompileResult (Maybe a))
allowNothing isNothing parser sexp = if isNothing sexp then pure Nothing else Just <$> parser sexp
-- pure Nothing
-- allowNothing f s = Just <$> f s

parseFor :: Sexp -> CompileResult (Maybe Stat, Maybe Expr, Maybe Stat, [Body])
parseFor (Atom _) = empty
parseFor (List (Atom "for" : rest)) = 
    if length rest < 3 then miscErr else
    let 
        init = (allowNothing (== List []) parseStat) (rest !! 0)
        cond = (allowNothing (== List []) parseExpr) (rest !! 1)
        update = (allowNothing (== List []) parseStat) (rest !! 2)
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