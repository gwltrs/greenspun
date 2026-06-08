module Type.Top where

import Type.Sexp

newtype Typed a = Typed (Sexp, a) deriving Show

data Lit
    = BoolLit Bool
    | IntLit Int
    | StringLit String
    deriving (Show, Eq)

-- data littyped
--     = boollittyped bool
--     | intlittyped int sexp
--     | stringlittyped string
--     deriving (Show, Eq)

data Expr
    = CallExpr [Expr]
    | LitExpr Lit
    | VarExpr String
    deriving (Show, Eq)

data ExprT
    = CallExprT [Typed Expr]
    | LitExprT (Typed Lit)
    | VarExprT (Typed String)

-- data ExprTyped
--     = CallExprTyped [Expr] Sexp
--     | LitExprTyped Lit Sexp
--     | VarExprTyped String Sexp
--     deriving (Show, Eq)

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