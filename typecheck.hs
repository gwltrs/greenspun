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
            -- (Typed . (, varName)) <$> intersectPossibleTypes expectedTypes (TheseTypes allEntries)

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

                    -- let
                    --     argTypes :: [[Either TypeCheckError (Typed Expr)]]
                    --     argTypes = possibleMatches <&> 
                    --         (\possibleMatch -> (flip mapListWithIndex) args 
                    --             (\i arg -> typeCheckExpr env (TheseTypes [argAtIndex i possibleMatch]) arg))         
                    --     argTypes' :: Either TypeCheckError [[Typed Expr]]
                    --     argTypes' = filterNonSevere argTypes -- (mapM sequence) argTypes -- sequence <$> sequence argTypes
                    -- in
                    --     case argTypes' of
                    --         (Left error) -> Left error
                    --         (Right argTypes'') -> undefined

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

--                         let 
--                             argTypes :: [Either TypeCheckError (Typed [Expr])]
--                             argTypes = (typeCheckExpr env expectedReturnType) <$> args
--                         in
--                             undefined
                        -- next filter based on immediately type-able variables

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

-- data PossibleTypes 
--     = AllTypes
--     | NonVoidTypes
--     | TheseTypes [Sexp] -- Should never be empty
--     deriving Show

-- We aren't implementing generics yet 
-- unionSpecificNonVoids :: [Sexp] -> [Sexp]
-- unionSpecificNonVoids types = foldr (union) [] types
    -- let 
    --     getArr :: PossibleTypes -> [Sexp]
    --     getArr (TheseTypes types') = types'
    --     getArr _ = []
    -- in 
    --     foldr (union . getArr) [] types

-- applyExpectedTypes :: PossibleTypes -> Typed a -> Either TypeCheckError (Typed a)
-- applyExpectedTypes expectedTypes (Typed (possibleTypes, inner)) =
--     case (expectedTypes, possibleTypes) of
--         (AllTypes, AllTypes) -> Typed (AllTypes, inner)
--         (NonVoidTypes, NonVoidTypes) -> Typed (NonVoidTypes, inner)
-- applyExpectedTypes Nothing r = r
-- applyExpectedTypes expectedTypes@(Just _) (Right (Typed (Nothing, inner))) = Right $ Typed (expectedTypes, inner)
-- applyExpectedTypes (Just expectedTypes) (Right (Typed (Just possibleTypes, inner))) =
--     case intersect expectedTypes (fromMaybe [] possibleTypes) of
--         [] -> Left $ ExpectedXsButGotYsError expectedTypes possibleTypes
--         valids -> Right (Typed (Just valids, inner))

-- data ExpectedReturnType
--     = ExpectingVoid
--     | ExpectingAnyNonVoid
--     | ExpectingTheseNonVoids [Sexp]
--     | ExpectingThisNonVoid Sexp

-- expectedReturnTypes :: ExpectedReturnType -> Maybe [Sexp]
-- expectedReturnTypes ExpectingVoid = Just [Atom "Void"]
-- expectedReturnTypes ExpectingAnyNonVoid = Nothing
-- expectedReturnTypes (ExpectingTheseNonVoids types) = Just types
-- expectedReturnTypes (ExpectingThisNonVoid type_) = Just [type_]

-- data ExpectedNonVoidReturnType
--     = ExpectingAny
--     | ExpectingThese [Sexp]
--     | ExpectingThis Sexp

-- expectedNonVoidReturnTypes :: ExpectedNonVoidReturnType -> Maybe [Sexp]
-- expectedNonVoidReturnTypes ExpectingAny = Nothing
-- expectedNonVoidReturnTypes (ExpectingThese types) = Just types
-- expectedNonVoidReturnTypes (ExpectingThis type_) = Just [type_]


-- typeCheckVar :: Env -> ExpectedNonVoidReturnType -> String -> Either TypeCheckError (Typed String)
-- typeCheckVar (Env map) expected varName = 
--     case expected of
--         ExpectingAny -> 
--             case 
--         (ExpectingThese types) -> undefined
--         (ExpectingThis type_) -> undefined







-- typeCheckVar (Env map) Nothing varName = 
--     case oneEntry =<< lookup varName map of
--     Just varType -> Right $ Typed (varType, varName)
--     Nothing -> Left $ NoValueWithNameError varName
-- typeCheckVar (Env map) (Just expected) varName =
--     let 
--         entries :: [Sexp]
--         entries = fromMaybe [] (allEntries <$> lookup varName map)
--     in 
--         case entries of
--             [] -> Left $ NoValueWithNameError varName
--             [entry] -> 
--                 if entry == expected 
--                 then Right $ Typed (entry, varName)
--                 else Left $ ExpectedXsButGotYsError [expected] [entry]
--             entries ->
--                 case filter (== expected) entries of
--                     [entry] -> Right $ Typed (entry, varName)
--                     _ -> Left $ ExpectedXsButGotYsError [expected] entries






-- data ExpectedReturnType
--     = ExpectingVoid
--     | ExpectingAnyNonVoid
--     | ExpectingTheseNonVoids [Sexp]
--     | ExpectingThisNonVoid Sexp

-- data TypeCheckError 
--     = ExpectedXButGotYsError Sexp [Sexp]
--     | CouldNotDetermineTypeOfXError Sexp
--     | NoValueWithNameError String
--     | CallMadeWithNonFunctionType Sexp
--     | NoFunctionWithThatArityOrReturnType String

-- typeCheckExpr :: Env -> ExpectedReturnType -> Expr -> Either TypeCheckError (Typed Expr)
-- typeCheckExpr _ expected (LitExpr lit) = 
--     let 
--         typedLit :: Typed Lit
--         typedLit = typeCheckLit lit
--         typeOfLit :: Sexp
--         typeOfLit = getType typedLit
--     in
--         case expectedReturnTypes expected of
--             Nothing -> Right (LitExpr <$> typedLit)
--             Just types_ ->
--                 if elem typeOfLit types_
--                 then Right (LitExpr <$> typedLit)
--                 else Left $ ExpectedXsButGotYsError types_ [typeOfLit]
-- typeCheckExpr env expected (VarExpr varName) = VarExpr <<$>> typeCheckVar env expected varName
-- typeCheckExpr env expected (CallExpr (fun : args)) = undefined

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