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
import Type.Top
import Type.Sexp
import Type.CompileResult
import Text.Read (readMaybe)
import Type.Parser.String 
import Parsers.String
import Parsers.Sexp
import Type.CompileResult
import Distribution.Simple.Utils (safeLast, safeInit)

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

tabs :: Int -> String
tabs i = replicate i '\t'

transpileType :: Sexp -> String
transpileType (Atom "Void") = "void"
transpileType (Atom "Bool") = "bool"
transpileType (Atom "Char") = "char"
transpileType (Atom "I32") = "int"
transpileType (Atom "F32") = "float"
transpileType (Atom t) = t
transpileType (List [Atom "*", inner]) = transpileType inner ++ "*"

transpileTop :: Int -> Top -> String
transpileTop i (FunTop _ _ _ _) = tabs i ++ "void asdf() {}"
transpileTop i (VarTop _ _ _) = tabs i ++ "int x = 0"

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

main :: IO ()
main = do
    sexpsM <- greenFilesSexps
    case sexpsM of
        Nothing -> putStrLn "Compilation Error"
        Just sexps -> 
            case sequence (parseTop <$> sexps) of
                CompileResult (Right (Just tops)) -> 
                    putStrLn $ unlines $ transpileTop 0 <$> tops
                _ -> putStrLn "Compilation Error"