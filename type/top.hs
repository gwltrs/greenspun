{-# LANGUAGE DeriveFunctor #-}

module Type.Top where

import Type.Sexp

-- newtype Typed a = Typed (PossibleTypes, a) deriving Show

data PossibleTypes 
    = AllTypes
    | NonVoidTypes
    | TheseTypes [Sexp] -- Should never be empty
    deriving Show

data Lit a
    = BoolLit a Bool
    | IntLit a Int
    | StringLit a String
    deriving (Show, Eq, Functor)

data Expr a
    = CallExpr [Expr a]
    | LitExpr (Lit a)
    | VarExpr a String
    deriving (Show, Eq)

data Body a
    = FunBody String [(String, Sexp)] Sexp [Body a]
    | RetBody (Maybe (Expr a))
    | VarBody [String] Sexp [Maybe (Expr a)]
    | IfBody [(Expr a, [Body a])] (Maybe [Body a])
    | ForBody (Maybe (Stat a)) (Maybe (Expr a)) (Maybe (Stat a)) [Body a]
    | CallBody [Expr a]
    deriving (Show, Eq)

data Stat a
    = VarStat [String] Sexp [Maybe (Expr a)]
    | CallStat [Expr a]
    deriving (Show, Eq)

data Top a
    = FunTop String [(String, Sexp)] Sexp [Body a]
    | VarTop [String] Sexp [Maybe (Expr a)] 
    | IncludeTop [String]
    deriving Show

class (Typeable t a) where
    getReturnType :: t a -> a

getTheseReturnTypes :: forall t. Typeable t PossibleTypes => t PossibleTypes -> [Sexp]
getTheseReturnTypes t =
    case getReturnType t of
        (TheseTypes types) -> types
        AllTypes -> error "getTheseTypes: found AllTypes instead"
        NonVoidTypes -> error "getTheseTypes: found NonVoidTypes instead"

getOneReturnType :: forall t. Typeable t PossibleTypes => t PossibleTypes -> Sexp
getOneReturnType t = 
    case getReturnType t of
        AllTypes -> error "getTheseTypes: found AllTypes instead"
        NonVoidTypes -> error "getTheseTypes: found NonVoidTypes instead"
        (TheseTypes []) -> error "getTheseTypes: found TheseTypes with zero types"
        (TheseTypes [t]) -> t
        (TheseTypes _) -> error "getTheseTypes: found TheseTypes with multiple types"

instance Typeable Lit a where
    getReturnType (BoolLit a _) = a
    getReturnType (IntLit a _) = a
    getReturnType (StringLit a _) = a

instance Typeable Expr a where
    getReturnType (CallExpr list) = getReturnType $ last list
    getReturnType (LitExpr lit) = getReturnType lit
    getReturnType (VarExpr a _) = a