module Type.Env where

import Type.Sexp
import qualified Data.Map as M
import Data.Maybe (fromJust)

data EnvEntry = VarEntry Sexp | FunsEntry [Sexp] deriving Show

newtype Env = Env (M.Map String EnvEntry)

-- data GlobalEnvEntry = GlobalVarEntry (Int, Sexp) | GlobalFunsEntry [(Int, Sexp)] deriving Show

-- newtype GlobalEnv = GlobalEnv (M.Map String EnvEntry)

addToEnv :: String -> EnvEntry -> Env -> Either (String, EnvEntry, EnvEntry) Env
addToEnv name entry (Env map) = 
    case M.lookup name map of
        Nothing -> Right $ Env $ M.insert name entry map
        Just found' -> 
            case (entry, found') of
                (FunsEntry l, FunsEntry r) ->
                    -- TODO: add check for obvious function conflicts
                    Right $ Env $ M.insert name (FunsEntry (l ++ r)) map
                _ -> 
                    Left (name, entry, found')

addManyToEnv :: [(String, EnvEntry)] -> Env -> Either (String, EnvEntry, EnvEntry) Env
addManyToEnv entries env = foldr folder (Right env) entries
    where
        folder :: (String, EnvEntry) -> Either (String, EnvEntry, EnvEntry) Env -> Either (String, EnvEntry, EnvEntry) Env
        folder _ (Left err) = Left err
        folder (name, entry) (Right env) = addToEnv name entry env

envFromMany :: [(String, EnvEntry)] -> Either (String, EnvEntry, EnvEntry) Env
envFromMany entries = addManyToEnv entries emptyEnv

envEntryFromSexp :: Sexp -> EnvEntry
envEntryFromSexp f@(List (Atom "Fun" : _)) = FunsEntry [f]
envEntryFromSexp s = VarEntry s

emptyEnv :: Env
emptyEnv = Env M.empty

oneEntry :: EnvEntry -> Maybe Sexp
oneEntry (VarEntry var) = Just var
oneEntry (FunsEntry [fun]) = Just fun
oneEntry (FunsEntry _) = Nothing

allEntries :: EnvEntry -> [Sexp]
allEntries (VarEntry var) = [var]
allEntries (FunsEntry funs) = funs