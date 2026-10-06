module Type.Top where

import Type.Sexp

newtype Typed a = Typed (PossibleTypes, a) deriving Show

getType :: Typed a -> Sexp
getType (Typed (TheseTypes [sexp], a)) = sexp

instance Functor Typed where
    fmap :: (a -> b) -> Typed a -> Typed b
    fmap f (Typed (types, value)) = Typed (types, f value)

data PossibleTypes 
    = AllTypes
    | NonVoidTypes
    | TheseTypes [Sexp] -- Should never be empty
    deriving Show

data Lit
    = BoolLit Bool
    | IntLit Int
    | StringLit String
    deriving (Show, Eq)

data Expr
    = CallExpr [Expr]
    | LitExpr Lit
    | VarExpr String
    deriving (Show, Eq)

data Body
    = FunBody String [(String, Sexp)] Sexp [Body]
    | RetBody (Maybe Expr)
    | VarBody [String] Sexp [Maybe Expr]
    | IfBody [(Expr, [Body])] (Maybe [Body])
    | ForBody (Maybe Stat) (Maybe Expr) (Maybe Stat) [Body]
    | CallBody [Expr]
    deriving (Show, Eq)

data Stat
    = VarStat [String] Sexp [Maybe Expr]
    | CallStat [Expr]
    deriving (Show, Eq)

data Top
    = FunTop String [(String, Sexp)] Sexp [Body]
    | VarTop [String] Sexp [Maybe Expr] 
    | IncludeTop [String]
    deriving Show