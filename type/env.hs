module Type.Env where

import Type.Sexp
import Data.Set

data Env = Env
    { varDecs :: Set VarDec
    , funDecs :: Set FunDec } 
    deriving Show

data VarDec = VarDec { varName :: String, varType :: Sexp } deriving Show

instance Eq VarDec where
    a == b = (varName a) == (varName b)

instance Ord VarDec where
    compare a b = compare (varName a) (varName b)

data FunDec = FunDec { funName :: String, funType :: Sexp } deriving Show

instance Eq FunDec where
    a == b = (funName a) == (funName b)

instance Ord FunDec where
    compare a b = compare (funName a) (funName b)