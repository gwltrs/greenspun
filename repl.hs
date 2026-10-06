module Repl where

import IO (greenFilesSexps)
import Type.Sexp (Sexp)
import Type.Top (Top)
import Data.Maybe (fromJust)
import Utils ((<<$>>), fromRight)
import Parsers.Sexp (parseTop)
import Type.CompileResult (fromCompileSuccess)
import PrettyGHCI (prettifyGHCI)

sexps :: IO [Sexp]
sexps = fromJust <$> greenFilesSexps

tops :: IO [Top]
tops = 
    let 
        asdf :: Sexp -> Top
        asdf = fromCompileSuccess . parseTop
    in
        asdf <<$>> sexps