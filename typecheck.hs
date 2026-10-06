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
import Type.Top (PossibleTypes(..))
import Data.Functor ((<&>))
import Data.Function ((&))

data TypeCheckError 
    = ExpectedXsButGotYsError PossibleTypes PossibleTypes
    -- | CouldNotDetermineTypeOfXError Sexp
    | NoValueWithNameError String
    | CallMadeWithNonFunctionType Sexp
    | NoFunctionWithThatArityOrReturnType String
    | NoFunctionWithThoseArgTypesError String
    | AmbiguousFunctionOverloadError String 

typeCheckLit :: Lit -> Typed Lit
typeCheckLit b@(BoolLit _) = Typed (TheseTypes [Atom "Bool"], b)
typeCheckLit i@(IntLit _) = Typed (TheseTypes [Atom "Int"], i)
typeCheckLit s@(StringLit _) = Typed (TheseTypes [List [Atom "*", Atom "Char"]], s)

typeCheckLit' :: PossibleTypes -> Lit -> Either TypeCheckError (Typed Lit)
typeCheckLit' expected = applyExpectedTypesToTyped expected . typeCheckLit

typeCheckVar :: Env -> PossibleTypes -> String -> Either TypeCheckError (Typed String)
typeCheckVar (Env map) expectedTypes varName = 
    case allEntries <$> lookup varName map of
        Nothing -> Left $ NoValueWithNameError varName
        Just allEntries' -> applyExpectedTypesToTyped expectedTypes (Typed (TheseTypes allEntries', varName)) 

typeCheckCall :: Env -> PossibleTypes -> [Expr] -> Either TypeCheckError (Typed [Expr])
typeCheckCall (Env map) _ ((LitExpr l) : args) = Left $ CallMadeWithNonFunctionType $ getType $ typeCheckLit l
typeCheckCall (Env map) _ (callExpr@(CallExpr funName) : args) = error "typeCheckCall: haven't implemented call typechecking when the function is produced by a function call"
typeCheckCall env@(Env map) expectedReturnType ve@((VarExpr v) : args) =
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
                [exactMatch] -> Right $ Typed (TheseTypes [fromJust $ returnType exactMatch], ve)
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

typeCheckExpr :: Env -> PossibleTypes -> Expr -> Either TypeCheckError (Typed Expr)
typeCheckExpr _ expected (LitExpr lit) = LitExpr <<$>> typeCheckLit' expected lit
typeCheckExpr env expected (VarExpr varName) = VarExpr <<$>> typeCheckVar env expected varName
typeCheckExpr env expected (CallExpr call) = CallExpr <<$>> typeCheckCall env expected call

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

applyExpectedTypesToTyped :: PossibleTypes -> Typed a -> Either TypeCheckError (Typed a)
applyExpectedTypesToTyped expected (Typed (actual, a)) = 
    case intersectPossibleTypes expected actual of
        Left error -> Left error
        Right types -> Right $ Typed (types, a)