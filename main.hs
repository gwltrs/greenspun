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

data Literal
    = BoolLiteral Bool
    | IntLiteral Int
    deriving Show

data Expression
    = CallExpression [Expression]
    | LiteralExpression Literal
    | VariableExpression String
    deriving Show

data Statement
    = FunctionStatement String [(String, Sexp)] Sexp [Statement]
    | ReturnStatement Expression
    | VariableStatement [String] Sexp Expression 
    | IfStatement [(Bool, [Statement])] (Maybe [Statement])
    | ForStatement (Maybe Statement) (Maybe Expression) (Maybe Statement) [Statement]
    deriving Show

miscErr :: CompileResult a
miscErr = CompileResult $ Left [MiscError]

------------------------------------------------------

parseLiteral :: Sexp -> CompileResult Literal
parseLiteral (Atom "true") = pure $ BoolLiteral True
parseLiteral (Atom "false") = pure $ BoolLiteral False
parseLiteral (Atom numText) =
    if isNum $ head numText
    then
        if all isNum numText
        then pure $ IntLiteral (read numText :: Int)
        else miscErr
    else empty
parseLiteral (List _) = CompileResult $ Right Nothing

parseCall :: Sexp -> CompileResult [Expression]
parseCall (Atom _) = empty
parseCall (List []) = miscErr
parseCall (List exprs) = mapM parseExpression exprs

parseIdentifier :: Sexp -> CompileResult String
parseIdentifier (Atom a) = pure a
parseIdentifier _ = empty

parseExpression :: Sexp -> CompileResult Expression
parseExpression s = 
    (CallExpression <$> parseCall s) 
        <|> (LiteralExpression <$> parseLiteral s)
        <|> (VariableExpression <$> parseIdentifier s)

------------------------------------------------------

parseFunction :: Sexp -> CompileResult (String, [(String, Sexp)], Sexp, [Statement])
parseFunction (Atom _) = empty
parseFunction (List ((Atom "fun") : (Atom name) : (List args) : rest))
    | odd $ length args = miscErr
    | any (\case (List _) -> True; (Atom _) -> False) $ fsts args = miscErr
    | hasArrow && (length rest == 1) = miscErr
    | otherwise = (name, argPairs, returnType, ) <$> statements
        where
            argPairs = (\l -> (fromAtom $ head l, l !! 1)) <$> chunk 2 args
            hasArrow = (rest !? 0) == Just (Atom "->")
            returnType = if hasArrow then rest !! 1 else Atom "Void"
            statementSexps = if hasArrow then drop 2 rest else rest
            statements = mapM parseStatement statementSexps
parseFunction (List (Atom "fun" : rest)) = miscErr
parseFunction _ = empty

parseReturn :: Sexp -> CompileResult Expression
parseReturn (List [Atom "return", expr]) = parseExpression expr
parseReturn (List (Atom "return" : expr)) = miscErr
parseReturn _ = empty

parseVariable :: Sexp -> CompileResult ([String], Sexp, Expression)
parseVariable (Atom _) = empty
parseVariable (List ((Atom "var") : rest)) =
    let len = length rest in
    if len < 2 || len > 3 then miscErr else
    let 
        names = elseCompileError MiscError (flatNotEmptyAtoms $ head rest)
        third = fromMaybe (List [Atom "init"]) (rest !? 2)
        expr = parseExpression third
    in
        (, rest !! 1, ) <$> names <*> expr
parseVariable _ = empty

parseIf :: Sexp -> CompileResult ([(Bool, [Statement])], Maybe [Statement])
parseIf = undefined

parseFor :: Sexp -> CompileResult (Maybe Statement, Maybe Expression, Maybe Statement, [Statement])
parseFor = undefined

parseStatement :: Sexp -> CompileResult Statement
parseStatement s = 
    (uncurry4 FunctionStatement <$> parseFunction s)
    <|> (ReturnStatement <$> parseReturn s)
    <|> (uncurry3 VariableStatement <$> parseVariable s)
    <|> (uncurry IfStatement <$> parseIf s)
    <|> (uncurry4 ForStatement <$> parseFor s)

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