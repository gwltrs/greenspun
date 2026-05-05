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
-- transpileType (List [Atom "->", inner]) = 

transpileTop :: Top -> String
transpileTop (FunTop name args returnType body) = transpileFun (name, args, returnType, body)
transpileTop (VarTop names type_ values) = transpileVar (names, type_, values)

transpileLit :: Lit -> String
transpileLit (IntLit i) = show i
transpileLit (BoolLit True) = "true"
transpileLit (BoolLit False) = "false"
transpileLit (StringLit s) = "\"" ++ s ++ "\""

transpileCall :: [Expr] -> String
transpileCall (fun : args) = transpileExpr fun ++ "(" ++ (intercalate ", " $ transpileExpr <$> args) ++ ")"
transpileCall _ = nonEx "transpileCall"

transpileStat :: Stat -> String
transpileStat (VarStat names type_ values) = transpileVar (names, type_, values)
transpileStat (CallStat exprs) = transpileCall exprs

transpileExpr :: Expr -> String
transpileExpr (CallExpr sexps) = transpileCall sexps
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

transpileFun :: (String, [(String, Sexp)], Sexp, [Body]) -> String
transpileFun (name, args, returnType, body) = transpileType returnType
    ++ " " 
    ++ mangleString name 
    ++ " ("
    ++ (intercalate ", " $ transpileArg <$> args)
    ++ ") { " 
    ++ transpileBodies body
    ++ " }"
        where
            transpileArg :: (String, Sexp) -> String
            transpileArg (argName, argType) = transpileType argType ++ " " ++ mangleString argName

transpileBodies :: [Body] -> String
transpileBodies bs = foldMap (++ "; ") $ transpileBody <$> bs

transpileFor :: ((Maybe Stat), (Maybe Expr), (Maybe Stat), [Body]) -> String
transpileFor (init, cond, update, body) = 
    let
        init' = fromMaybe "" (transpileStat <$> init)
        cond' = fromMaybe "" (transpileExpr <$> cond)
        update' = fromMaybe "" (transpileStat <$> update)
    in
        "for ("
            ++ init'
            ++ ";" 
            ++ cond' 
            ++ ";"
            ++ update'
            ++ ") { " 
            ++ transpileBodies body
            ++ "}" 

transpileIf :: ([(Expr, [Body])], (Maybe [Body])) -> String
transpileIf ((if_ : elseIfs), else_) =
    let 
        transpileIf :: (Expr, [Body]) -> String
        transpileIf (expr, body) = "if (" ++ transpileExpr expr ++ ") { " ++ transpileBodies body ++ " } "
        transpileElseIf :: (Expr, [Body]) -> String
        transpileElseIf (expr, body) = "else if (" ++ transpileExpr expr ++ ") { " ++ transpileBodies body ++ " } "
        transpileElse :: [Body] -> String
        transpileElse b = "else { " ++ transpileBodies b ++ " }"
    in
        transpileIf if_
            ++ (concat $ transpileElseIf <$> elseIfs) 
            ++ (fromMaybe "" $ transpileElse <$> else_)
transpileIf _ = nonEx "transpileIf"

transpileBody :: Body -> String
transpileBody (FunBody name args returnType body) = transpileFun (name, args, returnType, body)
transpileBody (RetBody Nothing) = "return"
transpileBody (RetBody (Just expr)) = "return " ++ transpileExpr expr
transpileBody (VarBody names type_ values) = transpileVar (names, type_, values)
transpileBody (IfBody conds else_) = transpileIf (conds, else_)
transpileBody (ForBody init cond update body) = transpileFor (init, cond, update, body)
transpileBody (CallBody exprs) = transpileCall exprs

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
        Nothing -> putStrLn "Compilation Error 1"
        Just sexps ->
            case sequence (parseTop <$> sexps) of
                CompileResult (Right (Just tops)) -> 
                    let cStr = unlines $ ((++ ";") . transpileTop) <$> tops
                    in writeFile "output.c" cStr
                    -- putStrLn $ 
                CompileResult (Left errs) -> putStrLn ("Errors: " ++ show errs)
                CompileResult _ -> nonEx "main"