module Type.GlobalEnv where

import Type.Sexp
import qualified Data.Map as M
import Type.Env
import Data.Sequence (mapWithIndex)
import Utils (mapListWithIndex)
import Type.Top

data GlobalEnvEntry = GlobalVarEntry Int Sexp | GlobalFunsEntry [(Int, Sexp)] deriving Show

data GlobalEnv = GlobalEnv Int (M.Map String GlobalEnvEntry)

globalEnvFromTops :: [Top a Int] -> Either (String, Sexp, Sexp) GlobalEnv
globalEnvFromTops tops = undefined

addToGlobalEnv :: String -> Sexp -> GlobalEnv -> Either (String, Sexp, Sexp) GlobalEnv
addToGlobalEnv name type_ (GlobalEnv nextID map) = 
    case M.lookup name map of
        Nothing -> 
            let 
                entry :: GlobalEnvEntry
                entry = 
                    if isFun type_
                    then GlobalFunsEntry [(nextID, type_)]
                    else GlobalVarEntry nextID type_
            in
                Right $ GlobalEnv (nextID + 1) (M.insert name entry map)
        Just found -> 
            case (found, isFun type_) of
                (GlobalVarEntry varID varType, _) -> Left (name, type_, varType)
                (GlobalFunsEntry funTypes, False) -> Left (name, type_, snd $ head funTypes)
                (GlobalFunsEntry funTypes, True) -> 
                    if elem type_ (snd <$> funTypes) -- TODO: generics need to be supported
                    then Left (name, type_, type_) 
                    else Right $ GlobalEnv (nextID + 1) (M.insert name (GlobalFunsEntry ((nextID, type_) : funTypes)) map)

emptyGlobalEnv :: GlobalEnv
emptyGlobalEnv = GlobalEnv 0 M.empty