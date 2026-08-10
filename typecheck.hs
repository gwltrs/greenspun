module TypeCheck where
    
import Type.Top
import Type.Sexp
import Type.Env
import Data.Map (lookup)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (intersect)
import Prelude hiding (lookup)
import Data.Foldable hiding (length)
import Utils ((<<$>>))
import Type.Top (PossibleTypes(..))

data TypeCheckError 
    = ExpectedXsButGotYsError PossibleTypes PossibleTypes
    -- | CouldNotDetermineTypeOfXError Sexp
    | NoValueWithNameError String
    | CallMadeWithNonFunctionType Sexp
    | NoFunctionWithThatArityOrReturnType String

typeCheckLit :: Lit -> Typed Lit
typeCheckLit b@(BoolLit _) = Typed (TheseTypes [Atom "Bool"], b)
typeCheckLit i@(IntLit _) = Typed (TheseTypes [Atom "Int"], i)
typeCheckLit s@(StringLit _) = Typed (TheseTypes [List [Atom "*", Atom "Char"]], s)

-- typeCheckVar :: Env -> PossibleTypes -> String -> Either TypeCheckError (Typed String)
-- typeCheckVar (Env map) expectedType varName = 
--     let 
--         entries :: [Sexp]
--         entries = fromMaybe [] (allEntries <$> lookup varName map)
--     in
--         case (expectedType, entries) of
--             (_, []) -> Left $ NoValueWithNameError varName
--             (Nothing, entries') -> Right $ Typed (Just entries', varName)
--             (Just expecteds, entries') -> 
--                 case intersect expecteds entries' of
--                     [] -> Left $ ExpectedXsButGotYsError expecteds entries'
--                     valids -> Right $ Typed (Just valids, varName)

-- typeCheckCall :: Env -> PossibleTypes -> [Expr] -> Either TypeCheckError (Typed [Expr])
-- typeCheckCall = undefined

-- typeCheckExpr :: Env -> PossibleTypes -> Expr -> Either TypeCheckError (Typed Expr)
-- typeCheckExpr (Env map) Nothing (LitExpr lit) = Right $ LitExpr <$> typeCheckLit lit
-- typeCheckExpr (Env map) (Just expecteds) (LitExpr lit) = 
--     case typeCheckLit lit of
--         t@(Typed (Nothing, lit')) -> Right $ LitExpr <$> t
--         (Typed (Just types, lit')) -> 
--             case intersect expecteds types of 
--                 [] -> Left $ ExpectedXsButGotYsError expecteds types
--                 valids -> Right $ Typed (Just valids, LitExpr lit')
-- typeCheckExpr (Env map) expectedType (VarExpr varName) = undefined
-- typeCheckExpr (Env map) expectedType (CallExpr exprs) = undefined

intersectPossibleTypes :: PossibleTypes -> PossibleTypes -> Either TypeCheckError PossibleTypes
intersectPossibleTypes AllTypes r = Right r
intersectPossibleTypes l AllTypes = Right l
intersectPossibleTypes NonVoidTypes NonVoidTypes = Right NonVoidTypes
intersectPossibleTypes l@NonVoidTypes r@(TheseTypes [Atom "Void"]) = Left $ ExpectedXsButGotYsError l r
intersectPossibleTypes NonVoidTypes (TheseTypes r) = Right $ TheseTypes $ filter (/= Atom "Void") r
intersectPossibleTypes l@(TheseTypes [Atom "Void"]) r@NonVoidTypes = Left $ ExpectedXsButGotYsError l r
intersectPossibleTypes (TheseTypes l) NonVoidTypes = Right $ TheseTypes $ filter (/= Atom "Void") l
intersectPossibleTypes l@(TheseTypes expected) r@(TheseTypes actual) = 
    case intersect expected actual of
        [] -> Left $ ExpectedXsButGotYsError l r
        valids -> Right $ TheseTypes valids

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






-- typeCheckCall :: Env -> ExpectedReturnType -> [Expr] -> Either TypeCheckError (Typed [Expr])
-- typeCheckCall (Env map) _ ((LitExpr l) : args) = Left $ CallMadeWithNonFunctionType $ getType $ typeCheckLit l
-- typeCheckCall (Env map) _ ((CallExpr funName) : args) = undefined
-- typeCheckCall env@(Env map) expectedReturnType ve@((VarExpr v) : args) = 
--     case lookup v map of
--         Nothing -> Left $ NoValueWithNameError v
--         Just (VarEntry sexp) -> Left $ CallMadeWithNonFunctionType sexp
--         Just (FunsEntry sexps) -> 
--             let 
--                 funsWithMatchingArity :: [Sexp]
--                 funsWithMatchingArity = filter (\s -> arity s == Just (length args)) sexps
--                 filterBasedOnReturnType :: ExpectedReturnType -> (Sexp -> Bool)
--                 filterBasedOnReturnType ert s = 
--                     case ert of
--                         ExpectingVoid -> s == Atom "Void"
--                         ExpectingAnyNonVoid -> s /= Atom "Void"
--                         ExpectingTheseNonVoids ts -> elem s ts
--                         ExpectingThisNonVoid t -> s == t
--                 funsWithMatchingReturnType :: [Sexp]
--                 funsWithMatchingReturnType = filter (filterBasedOnReturnType expectedReturnType) funsWithMatchingArity
--             in 
--                 case funsWithMatchingReturnType of
--                     [] -> Left $ NoFunctionWithThatArityOrReturnType v
--                     [exactMatch] -> Right $ Typed (fromJust $ returnType exactMatch, ve)
--                     possibleMatches -> 
--                         let 
--                             argTypes :: [Either TypeCheckError (Typed [Expr])]
--                             argTypes = (typeCheckExpr env expectedReturnType) <$> args
--                         in
--                             undefined
                        -- next filter based on immediately type-able variables

    -- filter by arg count

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