{-# LANGUAGE ScopedTypeVariables #-}
module Glob
(
  nameMatching
)
where

import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, getDirectoryContents)
import System.FilePath ((</>), dropTrailingPathSeparator, splitFileName)

import Control.Exception (handle, SomeException)
import Control.Monad (forM)
import GlobRegex (matchesGlob)

nameMatching :: String -> IO [FilePath]
nameMatching pat
  | not (isPattern pat) = do -- Simple case: check if is pattern. if not, check whether name exist
    exists <- doesNameExist pat
    return (if exists then [pat] else [])
  | otherwise = do -- It is a pattern
    case splitFileName pat of
      ("", baseName) -> do
        curDir <- getCurrentDirectory
        listMatches curDir baseName
      (dirName, baseName) -> do
        dirs <- if isPattern dirName
                  then nameMatching (dropTrailingPathSeparator dirName)
                  else return [dirName]
        let listDir = if isPattern baseName
                        then listMatches
                        else listPlain
        pathNames <- forM dirs $ \dir -> do
          baseNames <- listDir dir baseName
          return (map (dir </>) baseNames)
        return (concat pathNames)

isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  if fileExists
    then return True
    else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
  dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
  --handle (const (return [])) $ do -- this line don't work anymore since >= ghc 6.10
  -- this is because "handle" is
  -- handle :: Exception e => (e -> IO a) -> IO a -> IO a
  -- instead of
  -- handle :: (Exception -> IO a) -> IO a -> IO a
  -- the new type signature has a type variable, thus ambiguous
  handle (\(e::SomeException) -> (return [])) $ do
    names <- getDirectoryContents dirName'
    let names' = if isHidden pat
                  then filter isHidden names
                  else filter (not . isHidden) names
    return (filter (`matchesGlob` pat) names')

isHidden ('.':_) = True
isHidden _ = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <- if null baseName
              then doesDirectoryExist dirName
              else doesFileExist (dirName </> baseName)
  return (if exists then [baseName] else [])
