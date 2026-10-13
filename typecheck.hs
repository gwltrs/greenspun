module TypeCheck where
    
import Type.Top
import Type.Sexp
import Type.Env
import Data.Map (lookup)
import Data.Maybe (fromMaybe, fromJust, mapMaybe)
import Data.List (intersect, union, nub)
import Prelude hiding (lookup)
import Data.Foldable hiding (length)
import Utils ((<<$>>), mapListWithIndex, rightToMaybe, upTo)
import Type.Top
import Data.Functor ((<&>))
import Data.Function ((&))
import Control.Monad (when)

data TypeCheckError 
    = ExpectedXsButGotYsError PossibleTypes PossibleTypes
    -- | CouldNotDetermineTypeOfXError Sexp
    | NoValueWithNameError String
    | CallMadeWithNonFunctionType Sexp
    | NoFunctionWithThatArityOrReturnType String
    | NoFunctionWithThoseArgTypesError String
    | AmbiguousFunctionOverloadError String 

typeCheckLit :: Lit () -> Lit PossibleTypes
typeCheckLit (BoolLit () b) = BoolLit (TheseTypes [Atom "Bool"]) b
typeCheckLit (IntLit () i) = IntLit (TheseTypes [Atom "Int"]) i
typeCheckLit (StringLit () s) = StringLit (TheseTypes [List [Atom "*", Atom "Char"]]) s

typeCheckVar :: Env -> PossibleTypes -> String -> Either TypeCheckError (PossibleTypes, String)
typeCheckVar (Env map) expectedTypes varName = 
    case allEntries <$> lookup varName map of
        Nothing -> Left $ NoValueWithNameError varName
        Just allEntries' -> (, varName) <$> intersectPossibleTypes expectedTypes (TheseTypes allEntries')

typeCheckCall :: Env -> PossibleTypes -> [Expr ()] -> Either TypeCheckError [Expr PossibleTypes]
typeCheckCall (Env map) _ ((LitExpr l) : args) = Left $ CallMadeWithNonFunctionType $ getOneReturnType $ typeCheckLit l
typeCheckCall (Env map) _ (callExpr@(CallExpr funName) : args) = error "typeCheckCall: haven't implemented call typechecking when the function is produced by a function call"
typeCheckCall env@(Env map) expectedReturnType ve@((VarExpr () v) : args) = 
    case lookup v map of
        Nothing -> Left $ NoValueWithNameError v
        Just (VarEntry sexp) -> Left $ CallMadeWithNonFunctionType sexp
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
                [] -> Left $ NoFunctionWithThatArityOrReturnType v
                -- [exactMatch] -> Right $ Typed (TheseTypes [fromJust $ returnType exactMatch], ve)
                [exactMatch] -> Right (TheseTypes [fromJust $ returnType exactMatch], ve)

{-
            
                (possibleMatches :: [Sexp]) -> 
                    let 
                        typedArgs :: [Either TypeCheckError (Typed Expr)]
                        typedArgs = args & mapListWithIndex (\i arg -> 
                            let 
                                expectedTypes :: PossibleTypes
                                expectedTypes = TheseTypes $ nub $ argAtIndex i <$> possibleMatches
                            in 
                                typeCheckExpr env expectedTypes arg)
                    in
                        case sequence typedArgs of
                            Left error -> Left error
                            Right (typedArgs' :: [Typed Expr]) -> 
                                let 
                                    paramAndArgMatch :: Sexp -> Typed Expr -> Bool
                                    paramAndArgMatch s (Typed (possibleTypes, expr)) = 
                                        either (const False) (const True) $ intersectPossibleTypes (TheseTypes [s]) possibleTypes
                                    filterPossibleMatchesByArgType :: Sexp -> Bool
                                    filterPossibleMatchesByArgType param = 
                                        let
                                            argValidations :: [Bool]
                                            -- argValidations = typedArgs' <$> (\ta -> paramAndArgMatch  ta)
                                            argValidations = upTo (length typedArgs') 
                                                <&> (\i -> paramAndArgMatch (argAtIndex i param) (typedArgs' !! i))
                                        in
                                            and argValidations
                                in
                                    case filter filterPossibleMatchesByArgType possibleMatches of
                                        [] -> Left $ NoFunctionWithThoseArgTypesError v
                                        [exactMatch'] -> Right $ Typed (TheseTypes [fromJust $ returnType exactMatch'], ve)
                                        possibleMatches' -> Left $ AmbiguousFunctionOverloadError v

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

typeCheckExpr :: Env -> PossibleTypes -> Expr () -> Either TypeCheckError (Expr PossibleTypes)
typeCheckExpr _ expected (LitExpr lit) = Right $ LitExpr $ typeCheckLit lit
typeCheckExpr env expected (VarExpr () varName) = 
    case typeCheckVar env expected varName of
        Left err -> Left err
        Right (type_, varName') -> Right $ VarExpr type_ varName'

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
