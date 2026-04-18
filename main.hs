-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Parsers
import Data.List
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, size, toList)
import Utils
import System.IO
import Data.Functor (void, (<&>))
import Environment
import Text.Read (readMaybe)
import Parsers (unsafeAtom)

-- data CompRes

newtype CompileResult a = CompileResult (Either [CompileError] (Maybe a)) deriving Show

instance Functor CompileResult where
    -- fmap :: (a -> b) -> CompileResult a -> CompileResult b
    fmap f (CompileResult (Left errs)) = CompileResult $ Left errs
    fmap f (CompileResult (Right Nothing)) = CompileResult $ Right Nothing
    fmap f (CompileResult (Right (Just res))) = CompileResult $ Right $ Just $ f res

instance Applicative CompileResult where
    pure a = CompileResult $ Right $ Just a
    liftA2 f (CompileResult (Right (Just a))) (CompileResult (Right (Just b))) = CompileResult $ Right $ Just $ f a b
    liftA2 _ (CompileResult (Left errsA)) (CompileResult (Left errsB)) = CompileResult $ Left (errsA <> errsB)
    liftA2 _ (CompileResult (Left errs)) _ = CompileResult $ Left errs
    liftA2 _ _ (CompileResult (Left errs)) = CompileResult $ Left errs
    liftA2 _ _ _ = CompileResult $ Right Nothing

instance Monad CompileResult where
    (CompileResult (Right (Just res))) >>= f = f res
    (CompileResult (Right Nothing)) >>= _ = CompileResult $ Right Nothing
    (CompileResult (Left errs)) >>= _ = CompileResult $ Left errs

instance Alternative CompileResult where
    empty = CompileResult $ Right Nothing
    (CompileResult (Left errsA)) <|> (CompileResult (Left errsB)) = CompileResult $ Left (errsA <> errsB)
    (CompileResult (Left errs)) <|> _ = CompileResult $ Left errs
    _ <|> (CompileResult (Left errs)) = CompileResult $ Left errs
    (CompileResult (Right (Just res))) <|> _ = CompileResult $ Right $ Just res
    _ <|> (CompileResult (Right (Just res))) = CompileResult $ Right $ Just res
    _ <|> _ = CompileResult $ Right Nothing

data CompileError
    = MismatchError
    | MiscError
    deriving Show

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
    | VariableStatement String Expression
    | IfStatement [(Bool, [Statement])] (Maybe [Statement])
    | ForStatment Statement Expression Statement [Statement]
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

parseVariable :: Sexp -> CompileResult String
parseVariable (Atom a) = pure a
parseVariable _ = empty

parseExpression :: Sexp -> CompileResult Expression
parseExpression s = 
    (CallExpression <$> parseCall s) 
        <|> (LiteralExpression <$> parseLiteral s)
        <|> (VariableExpression <$> parseVariable s)

------------------------------------------------------

parseFunction :: Sexp -> CompileResult (String, [(String, Sexp)], Sexp, [Statement])
parseFunction (Atom _) = empty
parseFunction (List ((Atom "fun") : (Atom name) : (List args) : rest))
    | odd $ length args = miscErr
    | any (\case (List _) -> True; (Atom _) -> False) $ fsts args = miscErr
    | hasArrow && (length rest == 1) = miscErr
    | otherwise = (\ss -> (name, argPairs, returnType, ss)) <$> statements
        where
            argPairs = (\l -> (unsafeAtom $ head l, l !! 1)) <$> chunk 2 args
            hasArrow = (rest !? 0) == Just (Atom "->")
            returnType = if hasArrow then rest !! 1 else Atom "Void"
            statementSexps = if hasArrow then drop 2 rest else rest
            statements = mapM parseStatement statementSexps
parseFunction (List (Atom "fun" : rest)) = miscErr
parseFunction _ = empty

parseReturn :: Sexp -> CompileResult Expression
parseReturn (List [Atom "return", expr]) = parseExpression expr
    -- case parseExpression rest of
    -- else compErr MiscError
parseReturn (List (Atom "return" : expr)) = miscErr
parseReturn _ = empty

unc4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
unc4 f (a, b, c, d) = f a b c d

parseStatement :: Sexp -> CompileResult Statement
parseStatement s = 
    ((unc4 FunctionStatement) <$> parseFunction s)
    <|> (ReturnStatement <$> parseReturn s)

------------------------------------------------------

filePathSexps :: FilePath -> IO (Maybe [Sexp])
filePathSexps path = do
    text <- readFile path
    case runParser sexpsParser text of
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