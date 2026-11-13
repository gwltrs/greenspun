module TypeCheck where
    
import Type.Top
import Type.Sexp
import Type.Env
import Parsers.Sexp
import Data.Map (lookup)
import Data.Maybe (fromMaybe, fromJust, mapMaybe)
import Data.List (intersect, union, nub, singleton)
import Prelude hiding (lookup)
import Data.Foldable hiding (length)
import Utils ((<<$>>), mapListWithIndex, rightToMaybe, upTo, sequenceMaybes)
import Type.Top
import Data.Functor ((<&>))
import Data.Function ((&))
import Control.Monad (when)
import Type.Top (applyFunctionTypeToCall, voidType)
import ListT
import MaybeT
import Type.Env (addManyToEnv)
import Type.GlobalEnv

data TypeCheckError 
    = ExpectedXsButGotYsError PossibleTypes PossibleTypes
    -- | CouldNotDetermineTypeOfXError Sexp
    | NoValueWithNameError String
    | CallMadeWithNonFunctionTypeError Sexp
    | NoFunctionWithThatArityOrReturnTypeError String
    | NoFunctionWithThoseArgTypesError String
    | AmbiguousFunctionOverloadError String 
    | DuplicateVariableNameError String
    | VarNameAndFunNameConflictError String
        deriving Show

typeCheckLit :: Lit () -> Lit PossibleTypes
typeCheckLit (BoolLit () b) = BoolLit (TheseTypes [Atom "Bool"]) b
typeCheckLit (IntLit () i) = IntLit (TheseTypes [Atom "Int"]) i
typeCheckLit (StringLit () s) = StringLit (TheseTypes [List [Atom "*", Atom "Char"]]) s

typeCheckVarExpr :: Env -> PossibleTypes -> String -> Either TypeCheckError (PossibleTypes, String)
typeCheckVarExpr (Env map) expectedTypes varName = 
    case allEntries <$> lookup varName map of
        Nothing -> Left $ NoValueWithNameError varName
        Just allEntries' -> (, varName) <$> intersectPossibleTypes expectedTypes (TheseTypes allEntries')

typeCheckCall :: Env -> PossibleTypes -> [Expr ()] -> Either TypeCheckError [Expr PossibleTypes]
typeCheckCall (Env map) _ ((LitExpr l) : args) = Left $ CallMadeWithNonFunctionTypeError $ getOneReturnType $ typeCheckLit l
typeCheckCall (Env map) _ (callExpr@(CallExpr funName) : args) = error "typeCheckCall: haven't implemented call typechecking when the function is produced by a function call"
typeCheckCall env@(Env map) expectedReturnType ve@((VarExpr () funName) : args) = 
    case lookup funName map of
        Nothing -> Left $ NoValueWithNameError funName
        Just (VarEntry sexp) -> Left $ CallMadeWithNonFunctionTypeError sexp
        Just (FunsEntry sexps) -> 
            let
                funsWithMatchingArity :: [Sexp]
                funsWithMatchingArity = filter (\s -> arity s == Just (length args)) sexps
                filterBasedOnReturnType :: PossibleTypes -> (Sexp -> Bool)
                filterBasedOnReturnType AllTypes _ = True
                filterBasedOnReturnType NonVoidTypes s = s /= Atom "Void"
                filterBasedOnReturnType (TheseTypes types) s = elem s types
                funsWithMatchingReturnType :: [Sexp]
                funsWithMatchingReturnType = filter (filterBasedOnReturnType expectedReturnType) funsWithMatchingArity
            in case funsWithMatchingReturnType of
                [] -> Left $ NoFunctionWithThatArityOrReturnTypeError funName
                -- [exactMatch] -> Right $ Typed (TheseTypes [fromJust $ returnType exactMatch], ve)
                -- Right (TheseTypes [fromJust $ returnType exactMatch], ve)
                [exactMatch :: Sexp] -> Right $ (TheseTypes . singleton) <<$>> applyFunctionTypeToCall exactMatch ve
                (possibleMatches :: [Sexp]) -> 
                    let 
                        typedArgs :: [Either TypeCheckError (Expr PossibleTypes)]
                        typedArgs = args & mapListWithIndex (\i arg -> 
                            let 
                                expectedTypes :: PossibleTypes
                                expectedTypes = TheseTypes $ nub $ funTypeAtIndex i <$> possibleMatches
                            in 
                                typeCheckExpr env expectedTypes arg)
                    in
                        case sequence typedArgs of
                            Left error -> Left error
                            Right (typedArgs' :: [Expr PossibleTypes]) -> 
                                let 
                                    paramAndArgMatch :: Sexp -> Expr PossibleTypes -> Bool
                                    paramAndArgMatch s e = 
                                        either (const False) (const True) $ intersectPossibleTypes (TheseTypes [s]) (getReturnType e)
                                    filterPossibleMatchesByArgType :: Sexp -> Bool
                                    filterPossibleMatchesByArgType param = 
                                        let
                                            argValidations :: [Bool]
                                            argValidations = upTo (length typedArgs') 
                                                <&> (\i -> paramAndArgMatch (funTypeAtIndex i param) (typedArgs' !! i))
                                        in
                                            and argValidations
                                in
                                    case filter filterPossibleMatchesByArgType possibleMatches of
                                        [] -> Left $ NoFunctionWithThoseArgTypesError funName
                                        [exactMatch' :: Sexp] -> Right $ (TheseTypes . singleton) <<$>> applyFunctionTypeToCall exactMatch' ve
                                        possibleMatches' -> Left $ AmbiguousFunctionOverloadError funName

typeCheckExpr :: Env -> PossibleTypes -> Expr () -> Either TypeCheckError (Expr PossibleTypes)
typeCheckExpr _ expected (LitExpr lit) = Right $ LitExpr $ typeCheckLit lit
typeCheckExpr env expected (VarExpr () varName) = 
    case typeCheckVarExpr env expected varName of
        Left err -> Left err
        Right (type_, varName') -> Right $ VarExpr type_ varName'
typeCheckExpr env expected (CallExpr exprs) = CallExpr <$> typeCheckCall env expected exprs

typeCheckVarDef :: Env -> VarDef () -> Either TypeCheckError (Env, VarDef PossibleTypes)
typeCheckVarDef env (VarDef varNames varType varExprs) = 
    let 
        typedVarExprs :: [Maybe (Either TypeCheckError (Expr PossibleTypes))]
        typedVarExprs = typeCheckExpr env (TheseTypes [varType]) <<$>> varExprs
        typedVarExprs' :: Either TypeCheckError [Maybe (Expr PossibleTypes)]
        typedVarExprs' = sequenceMaybes typedVarExprs
        env' :: Either (String, EnvEntry, EnvEntry) Env
        env' = addManyToEnv ((, envEntryFromSexp varType) <$> varNames) env
    in 
        case typedVarExprs' of
            Left err -> Left err
            Right typedVarExprs'' -> 
                case env' of
                    Left (dupVarName, _, _) -> Left $ DuplicateVariableNameError dupVarName
                    Right env'' -> Right (env'', VarDef varNames varType typedVarExprs'')

typeCheckBody :: Env -> Sexp -> Body () -> Either TypeCheckError (Env, Body PossibleTypes)
typeCheckBody env returnType (FunBody _) = undefined
typeCheckBody env returnType (RetBody retExpr) = 
    case (returnType, retExpr) of
        (Atom "Void", Nothing) -> Right (env, RetBody Nothing)
        (Atom "Void", Just retExpr') -> Left $ ExpectedXsButGotYsError voidType NonVoidTypes
        (nonVoidReturnType, Nothing) -> Left $ ExpectedXsButGotYsError NonVoidTypes voidType
        (nonVoidReturnType, Just retExpr') -> (env, ) . RetBody . Just <$> typeCheckExpr env (thisType returnType) retExpr'
typeCheckBody env returnType (VarBody varDef) = VarBody <<$>> typeCheckVarDef env varDef
typeCheckBody env returnType (IfBody _ _) = undefined
typeCheckBody env returnType (ForBody _ _ _ _) = undefined
typeCheckBody env returnType (CallBody exprs) = (env, ) . CallBody <$> typeCheckCall env voidType exprs

typeCheckTop :: Env -> Top () Int -> Either TypeCheckError (Top PossibleTypes Int)
typeCheckTop env (FunTop _ _) = undefined 
typeCheckTop _ (IncludeTop includes id) = Right $ IncludeTop includes id
typeCheckTop env (VarTop varDef id) = (`VarTop` id) . snd <$> typeCheckVarDef env varDef

typeCheckStat :: Env -> Stat () -> Either TypeCheckError (Env, Stat PossibleTypes)
typeCheckStat env (VarStat varDef) = VarStat <<$>> typeCheckVarDef env varDef
typeCheckStat env (CallStat exprs) = (env, ) . CallStat <$> typeCheckCall env (TheseTypes [Atom "Void"]) exprs

globalEnv :: [Top () Int] -> Either TypeCheckError GlobalEnv
globalEnv tops = 
    let 
        env :: Either (String, Sexp, Sexp) GlobalEnv
        env = globalEnvFromTops tops
    in 
        case env of
            Right env -> Right env
            Left (name, leftType, rightType) ->
                case (isFun leftType, isFun rightType) of
                    (False, False) -> Left $ DuplicateVariableNameError name
                    (False, True) -> Left $ VarNameAndFunNameConflictError name
                    (True, False) -> Left $ VarNameAndFunNameConflictError name
                    (True, True) -> Left $ AmbiguousFunctionOverloadError name

-- topEnvEntry :: Top () -> [(String, EnvEntry)]
-- topEnvEntry (IncludeTop _) = []
-- topEnvEntry (VarTop varDef) = varDefEnvEntry varDef
-- topEnvEntry (FunTop funDef) = [funDefEnvEntry funDef]

-- varDefEnvEntry :: VarDef () -> [(String, EnvEntry)]
-- varDefEnvEntry = undefined

-- funDefEnvEntry :: FunDef () -> (String, EnvEntry)
-- funDefEnvEntry = undefined

-- data Stat a
--     = VarStat [String] Sexp [Maybe (Expr a)]
--     | CallStat [Expr a]
--     deriving (Show, Eq)

{-

filterNonSevere :: [[Either TypeCheckError (Typed Expr)]] -> Either TypeCheckError [[Typed Expr]]
filterNonSevere rows = 
    let 
        isNonSevere :: Either TypeCheckError [Typed Expr] -> Bool
        isNonSevere (Right _) = True
        isNonSevere (Left (ExpectedXsButGotYsError _ _)) = True
        isNonsever = False
        rows' :: [Either TypeCheckError [Typed Expr]]
        rows' = filter isNonSevere $ sequence <$> rows
    in 
        sequence rows' 

typeCheckExpr _ expected (LitExpr lit) = LitExpr <<$>> typeCheckLit' expected lit
typeCheckExpr env expected (VarExpr varName) = VarExpr <<$>> typeCheckVar env expected varName
typeCheckExpr env expected (CallExpr call) = CallExpr <<$>> typeCheckCall env expected call

-}

intersectPossibleTypes :: PossibleTypes -> PossibleTypes -> Either TypeCheckError PossibleTypes
intersectPossibleTypes AllTypes r = Right r
intersectPossibleTypes l AllTypes = Right l
intersectPossibleTypes NonVoidTypes NonVoidTypes = Right NonVoidTypes
intersectPossibleTypes l@NonVoidTypes r@(TheseTypes [Atom "Void"]) = Left $ ExpectedXsButGotYsError l r
intersectPossibleTypes NonVoidTypes (TheseTypes r) = Right $ TheseTypes $ filter (/= Atom "Void") r
intersectPossibleTypes l@(TheseTypes [Atom "Void"]) r@NonVoidTypes = Left $ ExpectedXsButGotYsError l r
intersectPossibleTypes (TheseTypes l) NonVoidTypes = Right $ TheseTypes $ filter (/= Atom "Void") l
intersectPossibleTypes l'@(TheseTypes l) r'@(TheseTypes r) = 
    case intersect l r of
        [] -> Left $ ExpectedXsButGotYsError l' r'
        valids -> Right $ TheseTypes valids

-- class Typeable t a where

-- applyExpectedTypes :: PossibleTypes -> (PossibleTypes, a) -> Either TypeCheckError (PossibleTypes, a)
-- applyExpectedTypes expected (actual, a) =
--     case intersectPossibleTypes expected actual of
--         Left err -> undefined
--         Right types -> undefined
        
    -- case intersectPossibleTypes expected actual of
    --     Left error -> Left error
    --     Right types -> Right $ Typed (types, a)