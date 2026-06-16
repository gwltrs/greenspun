module Type.Env where

import Type.Sexp
import qualified Data.Map as M
import Data.Maybe (fromJust)

data EnvEntry = VarEntry Sexp | FunsEntry [Sexp] deriving Show

newtype Env = Env (M.Map String EnvEntry) -- Env { varDecs :: Set VarDec, funDecs :: Set FunDec } deriving Show

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

envEntryFromSexp :: Sexp -> EnvEntry
envEntryFromSexp a@(Atom _) = VarEntry a
envEntryFromSexp f@(List (Atom "Fun" : _)) = FunsEntry [f]
envEntryFromSexp l@(List _) = VarEntry l

emptyEnv :: Env
emptyEnv = Env M.empty