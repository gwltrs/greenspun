{-# LANGUAGE LambdaCase #-}

module Utils where

import Data.Char (ord)

import Control.Monad (forM)
import System.Directory (listDirectory, getCurrentDirectory, doesDirectoryExist)
import System.FilePath (takeExtension, (</>))
import Data.List (isSuffixOf)
import Debug.Trace

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
combine (?) f g x = f x ? g x

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&&) = combine (&&)
infixr 3 &&&

(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(|||) = combine (||)
infixr 2 |||

{-# INLINABLE (!?) #-}
xs !? n
    | n < 0     = Nothing
    | otherwise = foldr (\x r k -> case k of
                                       0 -> Just x
                                       _ -> r (k-1)) (const Nothing) xs n

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk i xs = let (f, r) = splitAt i xs in f : chunk i r

fsts :: [a] -> [a]
fsts l = (!! 0) <$> chunk 2 l

snds :: [a] -> [a]
snds l = (!! 1) <$> chunk 2 l

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

splitAndKeepDelim :: (a -> Bool) -> [a] -> [[a]]
splitAndKeepDelim f l = filter (not . null) $ inner f l
    where
        inner _ [] = []
        inner p xs =
            let (chunk, rest) = break p xs
            in case rest of
                [] -> [chunk]
                (d:ds) ->
                    case inner p ds of
                        [] -> [chunk, [d]]
                        (r:rs) -> chunk : ((d:r) : rs)

traceLabel :: Show a => String -> a -> a
traceLabel l v = trace (l ++ ": " ++ show v) v

nonEx :: String -> a
nonEx s = error ("Runtime error due to non-exhaustive pattern matching in " ++ s)

(<<$>>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>>) = fmap . fmap

mapListWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapListWithIndex f xs = [f i x | (i, x) <- zip [0 ..] xs]

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Left _) = Nothing
rightToMaybe (Right b) = Just b

upTo :: Int -> [Int]
upTo i = [0 .. (i - 1)]

fromRight :: Either a b -> b
fromRight (Left _) = error "fromRight: found Left when expecting Right"
fromRight (Right b) = b

fromSingle :: [a] -> Maybe a
fromSingle [a] = Just a
fromSingle _ = Nothing