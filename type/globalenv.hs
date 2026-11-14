module Type.GlobalEnv where

import Type.Sexp
import qualified Data.Map as M
import Type.Env
import Data.Sequence (mapWithIndex)
import Utils (mapListWithIndex)
import Type.Top
import Type.Top (getFunDefType)
import Data.List (intersect, find)
import Data.Maybe (fromJust)

data GlobalEnvEntry = GlobalVarEntry Int Sexp | GlobalFunsEntry [(Int, Sexp)] deriving Show

data GlobalEnv = GlobalEnv Int (M.Map String GlobalEnvEntry)

globalEnvFromTops :: [Top () Int] -> Either (String, Sexp, Sexp) GlobalEnv
globalEnvFromTops tops = 
    let
        entries :: [(String, GlobalEnvEntry)]
        entries = topToEntry =<< tops 
    in
        undefined

topToEntry :: Top () Int -> [(String, GlobalEnvEntry)]
topToEntry (FunTop fd@(FunDef funName _ _ _) funID) = [(funName, GlobalFunsEntry [(funID, getFunDefType fd)])]
topToEntry (VarTop (VarDef varNames varType _) varID) = 
    let 
        mapVarName :: String -> (String, GlobalEnvEntry)
        mapVarName vn = 
            if isFun varType
            then (vn, GlobalFunsEntry [(varID, varType)])
            else (vn, GlobalVarEntry varID varType)
    in
        map mapVarName varNames
topToEntry (IncludeTop _ _) = []

addToGlobalEnv :: String -> GlobalEnvEntry -> GlobalEnv -> Either (String, Sexp, Sexp) GlobalEnv
addToGlobalEnv name entry (GlobalEnv nextID map) = 
    case M.lookup name map of
        Nothing -> Right $ GlobalEnv (nextID + 1) (M.insert name entry map)
        Just found -> 
            case (entry, found) of
                (GlobalVarEntry _ newVarType, GlobalVarEntry _ existingVarType) -> Left (name, newVarType, existingVarType)
                (GlobalVarEntry _ varType, GlobalFunsEntry ((_, funType) : _)) -> Left (name, varType, funType)
                (GlobalFunsEntry ((_, funType) : _), GlobalVarEntry _ varType) -> Left (name, funType, varType)
                (GlobalFunsEntry newFunTypes, GlobalFunsEntry existingFunTypes) ->
                    let (newFunTypes', existingFunTypes') = (snd <$> newFunTypes, snd <$> existingFunTypes)
                    in case intersect newFunTypes' existingFunTypes' of
                        conflict : _ -> Left (name, fromJust $ find (== conflict) newFunTypes', fromJust $ find (== conflict) existingFunTypes')
                        [] -> 
                            let newEntry = GlobalFunsEntry (newFunTypes ++ existingFunTypes)
                            in Right $ GlobalEnv nextID (M.insert name newEntry map)

-- addToGlobalEnv' :: String -> Sexp -> GlobalEnv -> Either (String, Sexp, Sexp) GlobalEnv
-- addToGlobalEnv' name type_ (GlobalEnv nextID map) = 
--     case M.lookup name map of
--         Nothing -> 
--             let 
--                 entry :: GlobalEnvEntry
--                 entry = 
--                     if isFun type_
--                     then GlobalFunsEntry [(nextID, type_)]
--                     else GlobalVarEntry nextID type_
--             in
--                 Right $ GlobalEnv (nextID + 1) (M.insert name entry map)
--         Just found -> 
--             case (found, isFun type_) of
--                 (GlobalVarEntry varID varType, _) -> Left (name, type_, varType)
--                 (GlobalFunsEntry funTypes, False) -> Left (name, type_, snd $ head funTypes)
--                 (GlobalFunsEntry funTypes, True) -> 
--                     if elem type_ (snd <$> funTypes) -- TODO: generics need to be supported
--                     then Left (name, type_, type_) 
--                     else Right $ GlobalEnv (nextID + 1) (M.insert name (GlobalFunsEntry ((nextID, type_) : funTypes)) map)

emptyGlobalEnv :: GlobalEnv
emptyGlobalEnv = GlobalEnv 0 M.empty