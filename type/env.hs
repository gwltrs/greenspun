module Type.Env where

import Type.Sexp
import Data.Set
import Data.Maybe (fromJust)

newtype EnvEntry = EnvEntry (String, Sexp) deriving Show

instance Eq EnvEntry where
    (==) :: EnvEntry -> EnvEntry -> Bool
    EnvEntry (leftName, leftType) == EnvEntry (rightName, rightType) = 
        if leftName /= rightName then False 
        else if (not $ isFun leftType) && (not $ isFun rightType) then True
        else leftType ~= rightType

instance Ord EnvEntry where
    compare :: EnvEntry -> EnvEntry -> Ordering
    compare l@(EnvEntry (ln, lt)) r@(EnvEntry (rn, rt)) =
        if l == r then EQ
        else if ln /= rn then compare ln rn
        else compare lt rt

newtype Env = Env (Set EnvEntry) -- Env { varDecs :: Set VarDec, funDecs :: Set FunDec } deriving Show

addToEnv :: EnvEntry -> Env -> Either (EnvEntry, EnvEntry) Env
addToEnv e (Env s) =
    if member e s
    then Left (e, fromJust $ lookupLE e s)
    else Right $ Env $ insert e s

emptyEnv :: Env
emptyEnv = Env empty

-- data VarDec = VarDec { varName :: String, varType :: Sexp } deriving Show

-- instance Eq VarDec where
--     a == b = (varName a) == (varName b)

-- instance Ord VarDec where
--     compare a b = compare (varName a) (varName b)

-- data FunDec = FunDec { funName :: String, funType :: Sexp } deriving Show

-- instance Eq FunDec where
--     a == b = (funName a) == (funName b)

-- instance Ord FunDec where
--     compare a b = compare (funName a) (funName b)