module TypeCheck where
    
import Type.Top
import Type.Sexp
import Type.Env

data TypeCheckError 
    = ExpectedXButGotYError Sexp Sexp
    | CouldNotDetermineTypeOfXError Sexp

typeCheckLit :: Lit -> Typed Lit
typeCheckLit b@(BoolLit _) = Typed (Atom "Bool", b)
typeCheckLit i@(IntLit _) = Typed (Atom "Int", i)
typeCheckLit s@(StringLit _) = Typed (List [Atom "*", Atom "Char"], s)

typeCheckVar :: Env -> Maybe Sexp -> String -> Typed String
typeCheckVar env Nothing varName = undefined

typeCheckExpr :: Env -> Maybe Sexp -> Expr -> Either TypeCheckError (Typed Expr)
typeCheckExpr env Nothing (LitExpr lit) = Right (LitExpr <$> typeCheckLit lit)
typeCheckExpr env (Just expected) (LitExpr lit) = 
    let tl@(Typed (type_, lit')) = typeCheckLit lit
    in 
        if expected == type_
        then Right (LitExpr <$> tl)
        else Left $ ExpectedXButGotYError expected type_

-- typeCheckExpr :: Env -> Maybe Sexp -> Expr -> ExprT
-- typeCheckExpr env expected (LitExpr lit) = 
-- typeCheckExpr env expected expr = undefined -- expected (CallExpr c) = undefined

-- data ExprT
--     = CallExprT [Typed Expr]
--     | LitExprT (Typed Lit)
--     | VarExprT (Typed String)