{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Parsers
import Data.List
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, size)
import Utils
import System.IO
import Data.Functor (void, (<&>))

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

data Env = Env { varDecs :: Set VarDec, funDecs :: Set FunDec } deriving Show

flatNotEmptyAtoms :: Sexp -> Maybe [String]
flatNotEmptyAtoms (Atom s) = Just [s]
flatNotEmptyAtoms (List []) = Nothing
flatNotEmptyAtoms (List l) = mapM (\case (List _) -> Nothing; (Atom s') -> Just s') l

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk i xs = let (f, r) = splitAt i xs in f : chunk i r

fsts :: [a] -> [a]
fsts l = (!! 0) <$> chunk 2 l

snds :: [a] -> [a]
snds l = (!! 1) <$> chunk 2 l

parseFunDec :: Sexp -> Maybe [FunDec]
parseFunDec (Atom _) = Just []
parseFunDec (List ((Atom "fun") : (Atom name): (List args: _))) = parseFunDec $ List ([Atom "fun", Atom "Void", Atom name, List args])
parseFunDec (List ((Atom "fun") : (returnType : ((Atom name): (List args: _)))))
    | odd $ length args = Nothing
    | any (\case (List _) -> True; (Atom _) -> False) $ snds args = Nothing
    | otherwise = Just $ [FunDec { funName = name, funType = List ([Atom "Fun", returnType] <> fsts args) }]
parseFunDec _ = Just []

parseVarDec :: Sexp -> Maybe [VarDec]
parseVarDec (Atom _) = Just []
parseVarDec (List ((Atom "var") : rest)) =
    let len = length rest in
    if len < 2 || len > 3 then Nothing else
    let names = flatNotEmptyAtoms (rest !! 1) in
    names <&> (\ns -> ns <&> (\n -> VarDec { varName = n, varType = head rest }))
parseVarDec _ = Just []

globalEnv :: [Sexp] -> Maybe Env
globalEnv ss = do
    funDecsList <- concat <$> sequence (parseFunDec <$> ss)
    varDecsList <- concat <$> sequence (parseVarDec <$> ss)
    let funDecsSet = fromList funDecsList
    let varDecsSet = fromList varDecsList
    if 
        length funDecsList == size funDecsSet &&
        length varDecsList == size varDecsSet
    then
        Just $ Env { varDecs = varDecsSet, funDecs = funDecsSet }
    else
        Nothing

filePathSexps :: FilePath -> IO (Maybe [Sexp])
filePathSexps path = do
    text <- readFile path
    case runParser sexpsParser text of
        Just (unparsed, sexps) -> pure (if unparsed == "" then Just sexps else Nothing)
        Nothing -> pure Nothing

greenFilesSexps :: IO (Maybe [Sexp])
greenFilesSexps = do
    paths <- findRelativeGreenFilePaths ""
    sexps <- traverse filePathSexps paths
    pure $ concat <$> sequence sexps

main :: IO ()
main = do
    sexps <- greenFilesSexps
    case sexps >>= globalEnv of
        Just env -> putStrLn $ show env
        Nothing -> putStrLn "Compilation Error"