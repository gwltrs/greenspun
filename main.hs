{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Parsers
import Data.List
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Utils

data CompileError = SyntaxError | MismatchError
data Var = Var { name :: String, type_ :: Sexp, body :: Sexp } deriving Show
newtype Env = Env { variables :: Set Var }

flatSymbols :: Sexp -> Maybe [String]
flatSymbols (List l) = mapM (\case (List l') -> Nothing; (Atom s') -> Just s') l
flatSymbols (Atom s) = Just [s]

-- isList :: Sexp -> Bool
-- isList (List _) = True
-- isList _ = False

var :: Sexp -> Maybe [Var]
var (Atom _) = Just []
var (List ((Atom "var") : sexps)) = 
    let len = length sexps; names = fromMaybe [] $ flatSymbols (sexps !! 1) in
    if len < 2 || len > 3 || null names then Nothing else
    let body = fromMaybe (List [Atom "default"]) (sexps !? 2) in
    Just ((\n -> Var { name = n, type_ = head sexps, body = body }) <$> names)
var _ = Just []

-- var (List [Atom "var", type_, Atom name]) = var $ List [Atom "var", type_, Atom name, List [Atom "default"]]

-- var (List [Atom "var", type_, Atom name, body]) = 
-- Nothing

globalEnv :: [Sexp] -> Env
globalEnv = undefined

funSig :: Sexp -> Maybe Sexp
funSig = undefined

varSig :: Sexp -> Maybe Sexp
varSig = undefined

main :: IO ()
main = undefined