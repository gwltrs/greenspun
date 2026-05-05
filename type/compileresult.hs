module Type.CompileResult where

import Control.Applicative
import Utils

newtype CompileResult a = CompileResult (Either [CompileError] (Maybe a)) deriving Show

instance Functor CompileResult where
    -- fmap :: (a -> b) -> CompileResult a -> CompileResult b
    fmap f (CompileResult (Left errs)) = CompileResult $ Left errs
    fmap f (CompileResult (Right Nothing)) = CompileResult $ Right Nothing
    fmap f (CompileResult (Right (Just res))) = CompileResult $ Right $ Just $ f res

instance Applicative CompileResult where
    pure a = CompileResult $ Right $ Just a
    liftA2 f (CompileResult (Right (Just a))) (CompileResult (Right (Just b))) = CompileResult $ Right $ Just $ f a b
    liftA2 _ (CompileResult (Left errsA)) (CompileResult (Left errsB)) = CompileResult $ Left (errsA <> errsB)
    liftA2 _ (CompileResult (Left errs)) _ = CompileResult $ Left errs
    liftA2 _ _ (CompileResult (Left errs)) = CompileResult $ Left errs
    liftA2 _ _ _ = CompileResult $ Right Nothing

instance Monad CompileResult where
    (CompileResult (Right (Just res))) >>= f = f res
    (CompileResult (Right Nothing)) >>= _ = CompileResult $ Right Nothing
    (CompileResult (Left errs)) >>= _ = CompileResult $ Left errs

instance Alternative CompileResult where
    empty = CompileResult $ Right Nothing
    (CompileResult (Left errsA)) <|> (CompileResult (Left errsB)) = CompileResult $ Left (errsA <> errsB)
    (CompileResult (Left errs)) <|> _ = CompileResult $ Left errs
    _ <|> (CompileResult (Left errs)) = CompileResult $ Left errs
    (CompileResult (Right (Just res))) <|> _ = CompileResult $ Right $ Just res
    _ <|> (CompileResult (Right (Just res))) = CompileResult $ Right $ Just res
    _ <|> _ = CompileResult $ Right Nothing

data CompileError
    = MismatchError
    | InvalidIntegerLiteralError
    | InvalidVarNamesError
    | ParamSexpsArentEvenError
    | ParamNameCantBeAListError
    | DanglingArrowInFunError
    | FoundFunButNoNameAtomAndParamsListError
    | InvalidNumberOfVarValuesError
    | EmptyIfStatementError
    | IfStatementIncorrectKeywordSequenceError
    | ForLoopTooFewSexpsError
    | MiscError
    deriving Show

elseCompileError :: CompileError -> Maybe a -> CompileResult a
elseCompileError _ (Just a) = pure a
elseCompileError err _ = CompileResult $ Left [err]

fromCompileSuccess :: CompileResult a -> a
fromCompileSuccess (CompileResult (Right (Just a))) = a
fromCompileSuccess _ = nonEx "fromCompileSuccess"

failCompile :: CompileError -> CompileResult a
failCompile = CompileResult . Left . pure