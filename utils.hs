module Utils where

import Data.Char (ord)

isLower :: Char -> Bool
isLower c = 97 <= ord c && ord c <= 122

isUpper :: Char -> Bool
isUpper c = 65 <= ord c && ord c <= 90

isAlpha :: Char -> Bool
isAlpha = isLower ||| isUpper

isNum :: Char -> Bool
isNum c = 48 <= ord c && ord c <= 57

isAlphaNum :: Char -> Bool
isAlphaNum = isAlpha ||| isNum

isVisible :: Char -> Bool
isVisible c = 33 <= ord c && ord c <= 126

isWhitespace :: Char -> Bool
isWhitespace c = let n = ord c in n == 9 || n == 10 || n == 13 || n == 32 

combine :: (b -> c -> d) -> (a -> b) -> (a -> c) -> (a -> d)
combine (?) f g  =  \x -> f x ? g x

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = combine (&&)
infixr 3 &&&

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = combine (||)
infixr 2 |||

-- (&&&?) :: (a -> Maybe Bool) -> (a -> Maybe Bool) -> a -> Maybe Bool
-- (&&&?) = combine (liftA2 (||))

-- (|||?) :: (a -> Maybe Bool) -> (a -> Maybe Bool) -> a -> Maybe Bool
-- (|||?) = combine (liftA2 (||))

{-# INLINABLE (!?) #-}
xs !? n
    | n < 0     = Nothing
    | otherwise = foldr (\x r k -> case k of
                                       0 -> Just x
                                       _ -> r (k-1)) (const Nothing) xs n