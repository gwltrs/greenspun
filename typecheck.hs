module TypeCheck where
    
import Type.Top
import Type.Sexp
import Type.Env
import Data.Map (lookup)
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)
import Utils ((<<$>>))

data TypeCheckError 
    = ExpectedXButGotYsError Sexp [Sexp]
    | CouldNotDetermineTypeOfXError Sexp
    | NoValueWithNameError String
    | CallMadeWithNonFunctionType Sexp

typeCheckLit :: Lit -> Typed Lit
typeCheckLit b@(BoolLit _) = Typed (Atom "Bool", b)
typeCheckLit i@(IntLit _) = Typed (Atom "Int", i)
typeCheckLit s@(StringLit _) = Typed (List [Atom "*", Atom "Char"], s)

typeCheckVar :: Env -> Maybe Sexp -> String -> Either TypeCheckError (Typed String)
typeCheckVar (Env map) Nothing varName = 
    case oneEntry =<< lookup varName map of
    Just varType -> Right $ Typed (varType, varName)
    Nothing -> Left $ NoValueWithNameError varName
typeCheckVar (Env map) (Just expected) varName =
    let 
        entries :: [Sexp]
        entries = fromMaybe [] (allEntries <$> lookup varName map)
    in 
        case entries of
            [] -> Left $ NoValueWithNameError varName
            [entry] -> 
                if entry == expected 
                then Right $ Typed (entry, varName)
                else Left $ ExpectedXButGotYsError expected [entry]
            entries ->
                case filter (== expected) entries of
                    [entry] -> Right $ Typed (entry, varName)
                    _ -> Left $ ExpectedXButGotYsError expected entries

typeCheckCall :: Env -> Maybe Sexp -> [Expr] -> Either TypeCheckError (Typed [Expr])
typeCheckCall (Env map) expected ((LitExpr l) : args) = Left $ CallMadeWithNonFunctionType $ getType $ typeCheckLit l
typeCheckCall (Env map) expected ((CallExpr funName) : args) = undefined
typeCheckCall (Env map) expected ((VarExpr v) : args) = undefined

typeCheckExpr :: Env -> Maybe Sexp -> Expr -> Either TypeCheckError (Typed Expr)
typeCheckExpr _ Nothing (LitExpr lit) = Right (LitExpr <$> typeCheckLit lit)
typeCheckExpr _ (Just expected) (LitExpr lit) = 
    let tl@(Typed (type_, lit')) = typeCheckLit lit
    in 
        if expected == type_
        then Right (LitExpr <$> tl)
        else Left $ ExpectedXButGotYsError expected [type_]
typeCheckExpr env expected (VarExpr varName) = VarExpr <<$>> typeCheckVar env expected varName
typeCheckExpr env expected (CallExpr (fun : args)) = undefined



-- typeCheckExpr (Env map) Nothing varExpr@(VarExpr varName) = 
--     case oneEntry =<< lookup varName map of
--         Just varType -> Right $ Typed (varType, varExpr)
--         Nothing -> Left $ NoValueWithNameError varName
-- typeCheckExpr (Env map) (Just expected) (VarExpr var) = undefined

-- typeCheckExpr :: Env -> Maybe Sexp -> Expr -> ExprT
-- typeCheckExpr env expected (LitExpr lit) = 
-- typeCheckExpr env expected expr = undefined -- expected (CallExpr c) = undefined

-- data ExprT
--     = CallExprT [Typed Expr]
--     | LitExprT (Typed Lit)
--     | VarExprT (Typed String)