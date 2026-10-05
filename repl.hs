module Repl where

import IO (greenFilesSexps)
import Type.Sexp
import Data.Maybe (fromJust)

gfs :: IO [Sexp]
gfs = fromJust <$> greenFilesSexps