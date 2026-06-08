module TypeCheck where
    
import Type.Top
import Type.Sexp
import Type.Env

typeCheckLit :: Lit -> Typed Lit
typeCheckLit b@(BoolLit _) = Typed (Atom "Bool", b)
typeCheckLit i@(IntLit _) = Typed (Atom "Int", i)
typeCheckLit s@(StringLit _) = Typed (List [Atom "*", Atom "Char"], s)

typeCheckExpr :: Env -> Maybe Sexp -> Expr -> ExprT
typeCheckExpr env expected expr = undefined -- expected (CallExpr c) = undefined

-- data ExprT
--     = CallExprT [Typed Expr]
--     | LitExprT (Typed Lit)
--     | VarExprT (Typed String)