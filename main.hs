{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Parsers
import Data.List
import Data.Maybe (fromMaybe)
import Data.Set (Set, fromList, size, toList)
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
    a == b = (funName a) == (funName b)

instance Ord FunDec where
    compare a b = compare (funName a) (funName b)

data Env = Env { varDecs :: Set VarDec, funDecs :: Set FunDec } deriving Show

flatNotEmptyAtoms :: Sexp -> Maybe [String]
flatNotEmptyAtoms (Atom s) = Just [s]
flatNotEmptyAtoms (List []) = Nothing
flatNotEmptyAtoms (List l) = mapM (\case (List _) -> Nothing; (Atom s') -> Just s') l

parseFunDec :: Sexp -> Maybe [FunDec]
parseFunDec (Atom _) = Just []
parseFunDec (List ((Atom "fun") : (Atom name) : (List args) : rest))
    | odd $ length args = Nothing
    | any (\case (List _) -> True; (Atom _) -> False) $ fsts args = Nothing
    | hasArrow && (length rest == 1) = Nothing
    | otherwise = Just [FunDec { funName = name, funType = List ([Atom "->"] <> snds args <> [returnType]) }]
        where
            hasArrow = (rest !? 0) == Just (Atom "->")
            returnType = if hasArrow then rest !! 1 else Atom "Void"
-- parseFunDec (List ((Atom "fun") : (returnType : ((Atom name): (List args: _)))))
--     | odd $ length args = Nothing
--     | any (\case (List _) -> True; (Atom _) -> False) $ snds args = Nothing
--     | otherwise = Just $ [FunDec { funName = name, funType = List ([Atom "Fun", returnType] <> fsts args) }]
parseFunDec (List (Atom "fun" : rest)) = Nothing
parseFunDec _ = Just []

parseVarDec :: Sexp -> Maybe [VarDec]
parseVarDec (Atom _) = Just []
parseVarDec (List ((Atom "var") : rest)) =
    let len = length rest in
    if len < 2 || len > 3 then Nothing else
    let names = flatNotEmptyAtoms $ head rest in
    names <&> (\ns -> ns <&> (\n -> VarDec { varName = n, varType = rest !! 1 }))
parseVarDec _ = Just []

globalEnv :: [Sexp] -> Maybe Env
globalEnv ss = do
    funDecsList <- concat <$> mapM parseFunDec ss
    varDecsList <- concat <$> mapM parseVarDec ss
    let funDecsSet = fromList funDecsList
    let varDecsSet = fromList varDecsList
    let nameSet = fromList ((funName <$> toList funDecsSet) <> (varName <$> toList varDecsSet))
    if 
        length funDecsList == size funDecsSet &&
        length varDecsList == size varDecsSet &&
        length nameSet == length funDecsList + length varDecsList
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