{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Parsers
import Data.List
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Utils

data CompileError = SyntaxError | MismatchError
data Fun = Fun { funName :: String, funType :: Sexp, funBody :: [Sexp] } deriving Show
data Var = Var { varName :: String, varType :: Sexp, varBody :: Sexp } deriving Show
data Env = Env { vars :: Set Var, funs :: Set Fun }

flatSymbols :: Sexp -> Maybe [String]
flatSymbols (List l) = mapM (\case (List l') -> Nothing; (Atom s') -> Just s') l
flatSymbols (Atom s) = Just [s]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk i xs = let (f, r) = splitAt i xs in f : chunk i r

fsts :: [a] -> [a]
fsts l = (!! 0) <$> chunk 2 l

snds :: [a] -> [a]
snds l = (!! 1) <$> chunk 2 l

parseFun :: Sexp -> Maybe Fun
parseFun (Atom _) = Nothing
parseFun (List ((Atom "fun") : (Atom name): (List args: body))) = parseFun $ List ([Atom "fun", Atom "Void", Atom name, List args] <> body)
parseFun (List ((Atom "fun") : (returnType : ((Atom name): (List args: body)))))
    | null body && returnType /= Atom "Void" = Nothing
    | odd $ length args = Nothing
    | any (\case (List _) -> True; (Atom _) -> False) $ snds args = Nothing
    | otherwise = Just $ Fun { funName = name, funType = List ([Atom "Fun", returnType] <> fsts args), funBody = body }
parseFun _ = Nothing

parseVar :: Sexp -> Maybe [Var]
parseVar (Atom _) = Just []
parseVar (List ((Atom "var") : sexps)) = 
    let len = length sexps; names = fromMaybe [] $ flatSymbols (sexps !! 1) in
    if len < 2 || len > 3 || null names then Nothing else
    let body = fromMaybe (List [Atom "default"]) (sexps !? 2) in
    Just ((\n -> Var { varName = n, varType = head sexps, varBody = body }) <$> names)
parseVar _ = Just []

-- var (List [Atom "var", type_, Atom name]) = var $ List [Atom "var", type_, Atom name, List [Atom "default"]]
-- var (List [Atom "var", type_, Atom name, body]) = 
-- Nothing

globalEnv :: [Sexp] -> Env
globalEnv = undefined

funSig :: Sexp -> Maybe Sexp
funSig = undefined

varSig :: Sexp -> Maybe Sexp
varSig = undefined

main :: IO ()
main = undefined