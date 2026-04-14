{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Parsers
import Data.List
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Utils
import System.IO
import Data.Functor (void)

data CompileError = SyntaxError | MismatchError

data VarDec = VarDec { varName :: String, varType :: Sexp } deriving Show

instance Eq VarDec where
    a == b = (varName a) == (varName b)

instance Ord VarDec where
    compare a b = compare (varName a) (varName b)

data FunDec = FunDec { funName :: String, funType :: Sexp } deriving Show

instance Eq FunDec where
    a == b = (funName a) == (funName b) && (funType a) == (funType b)

instance Ord FunDec where
    compare a b = compare (funName a) (funName b)

data Env = Env { varDecs :: Set VarDec, funDecs :: Set FunDec }

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

parseFunDec :: Sexp -> Maybe FunDec
parseFunDec (Atom _) = Nothing
parseFunDec (List ((Atom "fun") : (Atom name): (List args: _))) = parseFunDec $ List ([Atom "fun", Atom "Void", Atom name, List args])
parseFunDec (List ((Atom "fun") : (returnType : ((Atom name): (List args: _)))))
    | odd $ length args = Nothing
    | any (\case (List _) -> True; (Atom _) -> False) $ snds args = Nothing
    | otherwise = Just $ FunDec { funName = name, funType = List ([Atom "Fun", returnType] <> fsts args) }
parseFunDec _ = Nothing

parseVarDec :: Sexp -> Maybe [VarDec]
parseVarDec (Atom _) = Just []
parseVarDec (List ((Atom "var") : sexps)) = 
    let len = length sexps; names = fromMaybe [] $ flatSymbols (sexps !! 1) in
    if len < 2 || len > 3 || null names then Nothing else
    let body = fromMaybe (List [Atom "default"]) (sexps !? 2) in
    Just ((\n -> VarDec { varName = n, varType = head sexps }) <$> names)
parseVarDec _ = Just []

globalEnv :: [Sexp] -> Maybe Env
globalEnv = undefined

main :: IO ()
main = do
    greenFilePaths <- findRelativeGreenFilePaths ""
    greenFileContents <- sequence (readFile <$> greenFilePaths)
    void $ sequence (putStrLn <$> greenFileContents)