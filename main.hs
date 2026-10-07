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
import TypeCheck
import IO (greenFileSexps)
import Repl

main :: IO ()
main = do
    sexpsM <- greenFileSexps
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