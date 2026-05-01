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
import Data.Bifunctor (second)
import Data.Char (ord)

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

transpileLit :: Lit -> String
transpileLit (IntLit i) = show i
transpileLit (BoolLit True) = "true"
transpileLit (BoolLit False) = "false"

transpileExpr :: Expr -> String
transpileExpr (CallExpr (fun : args)) = transpileExpr fun ++ "(" ++ (intercalate ", " $ transpileExpr <$> args) ++ ")"
transpileExpr (LitExpr l) = transpileLit l
transpileExpr (VarExpr v) = mangleString v

transpileVar :: ([String], Sexp, [Maybe Expr]) -> String
transpileVar ([name], type_, [Nothing]) = transpileType type_ ++ " " ++ mangleString name
transpileVar ([name], type_, [Just value]) = transpileType type_ ++ " " ++ mangleString name ++ " = " ++ transpileExpr value
transpileVar (names, type_, values) =
    let
        (strippedType, ptrs) = stripPointrs $ transpileType type_
        zipped :: [(String, Maybe Expr)]
        zipped = zip names values
        mapRow :: (String, Maybe Expr) -> String
        mapRow (name, Nothing) = replicate ptrs '*' ++ mangleString name
        mapRow (name, Just expr) = replicate ptrs '*' ++ mangleString name ++ " = " ++ transpileExpr expr
    in
        strippedType ++ " " ++ (intercalate ", " (mapRow <$> zipped))


tabs :: Int -> String
tabs i = replicate i '\t'

stripPointrs :: String -> (String, Int)
stripPointrs = second length . break (== '*')

mangleFun :: String -> String
mangleFun s = "gsfun_" ++ mangleString s

mangleVar :: String -> String
mangleVar s = "gsvar_" ++ mangleString s

mangleString :: String -> String
mangleString = (>>= mangleChar)

mangleChar :: Char -> String
mangleChar c =
    if c == '_' then 
        "__"
    else if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') || c == '_' then
        [c]
    else
        "_" ++ (show $ ord c)

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