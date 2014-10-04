module RecursiveContents
(
  getRecursiveContents
)
where

import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

getRecursiveContents :: FilePath -> IO [FilePath]

getRecursiveContents topdir = do
  names <- getDirectoryContents topdir
  let properNames = filter (`notElem` [".", ".."]) names
  -- check out the types of forM and mapM
  -- notice there are very similar except for the positions
  -- of their arguments
  -- the reason we use forM is that we want to put the function at the end
  paths <- forM properNames $ \name -> do
    -- the (</>) joins the dir and name: e.g. topdir/name
    let path = topdir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory
        then getRecursiveContents path
        else return [path]
  return (concat paths)
