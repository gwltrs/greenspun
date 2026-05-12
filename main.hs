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
import Transpile

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
                    writeFile "output.c" (transpileAll tops)
                    -- putStrLn $ 
                CompileResult (Left errs) -> putStrLn ("Errors: " ++ show errs)
                CompileResult (Right Nothing) -> putStrLn "Failed to parse"
                -- CompileResult _ -> nonEx "main"