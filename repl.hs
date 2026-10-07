module Repl where

import Prelude hiding (read)
import IO (fileNameSexps, greenFileSexps, findRelativeGreenFilePaths, findGreenFilePathByName)
import System.IO (readFile)
import Type.Sexp (Sexp)
import Type.Top (Top)
import Type.Parser.String (runParser)
import Data.Maybe (fromJust)
import Utils ((<<$>>), fromRight)
import Parsers.Sexp (parseTop)
import qualified Parsers.String as PS
import Type.CompileResult (fromCompileSuccess, CompileResult(..), CompileError)
import PrettyGHCI (prettifyGHCI)

file :: String -> IO String
file name = do
    path <- findGreenFilePathByName name
    case path of
        Nothing -> error ("No, or duplicate, file named '" ++ name ++ ".green'")
        Just path' -> readFile path'

sexps :: String -> IO [Sexp]
sexps name = do
    text <- file name
    case runParser PS.sexps text of
        Nothing -> error "Failed to parse sexps"
        Just (remainingText, sexps') -> 
            if null remainingText
            then pure sexps'
            else error ("Parsed sexps, but found text remaining: " ++ remainingText)

tops :: String -> IO [Top]
tops name = do
    (sexps' :: [Sexp]) <- sexps name
    ((CompileResult res) :: CompileResult [Top]) <- pure $ mapM parseTop sexps'
    case res of
        Left (errs :: [CompileError]) -> error ("Failed to parse tops with errors: " ++ (unlines $ show <$> errs))
        Right (tops' :: Maybe [Top]) -> 
            case tops' of
                Nothing -> error "Failed to find tops to parse"
                Just (tops'' :: [Top]) -> pure tops''