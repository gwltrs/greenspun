{-# LANGUAGE LambdaCase #-}

module Utils where

import Data.Char (ord)

import Control.Monad (forM)
import System.Directory (listDirectory, getCurrentDirectory, doesDirectoryExist)
import System.FilePath (takeExtension, (</>))
import Data.List (isSuffixOf)

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

findRelativeGreenFilePaths :: FilePath -> IO [FilePath]
findRelativeGreenFilePaths rel = do
    let dir = if null rel then "." else rel
    contents <- listDirectory dir
    fmap concat $ forM contents $ \name -> do
        let path = dir </> name
        let relPath = if null rel then name else rel </> name
        isDir <- doesDirectoryExist path
        if isDir
            then findRelativeGreenFilePaths relPath
            else pure [relPath | takeExtension name == ".green"]

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