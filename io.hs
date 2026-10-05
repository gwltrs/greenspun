module IO where

import Type.Sexp (Sexp)
import System.IO
import System.Directory (listDirectory, getCurrentDirectory, doesDirectoryExist)
import System.FilePath (takeExtension, (</>))
import Data.List (isSuffixOf)
import Control.Monad (forM)
import Type.Parser.String
import Parsers.String (sexps)

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

filePathSexps :: FilePath -> IO (Maybe [Sexp])
filePathSexps path = do
    text <- readFile path
    case runParser sexps text of
        Just (unparsed, sexps) -> pure (if unparsed == "" then Just sexps else Nothing)
        Nothing -> pure Nothing

greenFilesSexps :: IO (Maybe [Sexp])
greenFilesSexps = do
    paths <- findRelativeGreenFilePaths ""
    sexps <- traverse filePathSexps paths
    pure $ concat <$> sequence sexps