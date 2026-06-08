{-# LANGUAGE LambdaCase #-}

module Type.Sexp where

import Utils

data Sexp = List [Sexp] | Atom String deriving (Eq, Ord)

type Type = Sexp

instance Show Sexp where
    show (Atom a) = a
    show (List l) = "(" <> (unwords (show <$> l)) <> ")"

fromAtom :: Sexp -> String
fromAtom (Atom s) = s
fromAtom _ = nonEx "fromAtom"

flatNotEmptyAtoms :: Sexp -> Maybe [String]
flatNotEmptyAtoms (Atom s) = Just [s]
flatNotEmptyAtoms (List []) = Nothing
flatNotEmptyAtoms (List l) = mapM (\case (List _) -> Nothing; (Atom s') -> Just s') l

(~=) :: Sexp -> Sexp -> Bool
(List l) ~= (List r) =
    if length l /= length r then False
    else and $ uncurry (~=) <$> zip l r
(Atom l) ~= (Atom r) =
    if all isUpper l /= all isUpper r then False
    else if all isUpper l then True
    else l == r
_ ~= _ = False

isFun :: Sexp -> Bool
isFun (List (Atom "Fun" : _)) = True
isFun _ = False