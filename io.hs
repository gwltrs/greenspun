module IO where

import Type.Sexp (Sexp)
import System.IO
import System.Directory (listDirectory, getCurrentDirectory, doesDirectoryExist)
import System.FilePath (takeExtension, (</>), takeFileName)
import Data.List (isSuffixOf)
import Data.Functor ((<&>))
import Control.Monad (forM, (=<<))
import Type.Parser.String
import Parsers.String (sexps)
import Utils (fromSingle)

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
            
findGreenFilePathByName :: String -> IO (Maybe FilePath)
findGreenFilePathByName name = findRelativeGreenFilePaths ""
    <&> filter (\fp -> takeFileName fp == (name ++ ".green"))
    <&> fromSingle

filePathSexps :: FilePath -> IO (Maybe [Sexp])
filePathSexps path = do
    text <- readFile path
    case runParser sexps text of
        Just (unparsed, sexps) -> pure (if unparsed == "" then Just sexps else Nothing)
        Nothing -> pure Nothing

filePathsSexps :: [FilePath] -> IO (Maybe [Sexp])
filePathsSexps paths = do
    sexps <- traverse filePathSexps paths
    pure $ concat <$> sequence sexps

fileNameSexps :: String -> IO (Maybe [Sexp])
fileNameSexps name = do
    path <- findGreenFilePathByName name
    case path of
        Nothing -> Nothing <$ putStrLn ("No, or duplicate, file named '" ++ name ++ ".green'")
        Just path' -> filePathSexps path'

greenFileSexps :: IO (Maybe [Sexp])
greenFileSexps = filePathsSexps =<< findRelativeGreenFilePaths ""

greenFileSexpsIn :: FilePath -> IO (Maybe [Sexp])
greenFileSexpsIn fp = filePathsSexps =<< findRelativeGreenFilePaths fp