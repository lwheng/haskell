{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}
module ControlledVisit
where

import Control.Monad (filterM,forM,liftM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.Time (ClockTime(..)) -- getModificationTime does not return a ClockTime. That's why we import UTCTime and changed our Predicate accordingly
import Data.Time.Clock (UTCTime)
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import RecursiveContents (getRecursiveContents)

data Info = Info {
   infoPath :: FilePath
 , infoPerms :: Maybe Permissions
 , infoSize :: Maybe Integer
 , infoModTime :: Maybe UTCTime
} deriving (Eq, Ord, Show)

-- When traversing the filesystem, we want more control on which directories to enter or not. An easy way is to pass on a function that takes a list of subdirectories of a given directory and returns another list (smaller or equal to the original)

-- Look at the type signature
-- Take a function, a path and outputs an IO
traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  -- get all contents from given "path", removing "." and ".."
  names <- getUsefulContents path
  -- for each name, prepend them with "path". Add path to the collection
  -- then for each of then, apply getInfo to get a collection of Info objects
  contents <- mapM getInfo (path : map (path </>) names)
  -- Now, for each of "contents" after applying "order",
  -- perform that lambda function to either recurse or return [current]
  -- finally, from the results of forM, of type IO [[Info]],
  -- we perform liftM: takes output of forM out of IO monad, applies concat, and then wrap in IO again
  -- liftM :: Monad m => (a1 -> r) -> m a1 -> m r
  -- a1 == [[Info]]
  -- m == IO
  -- r == [Info]
  liftM concat $ forM (order contents) $ \info ->
    if isDirectory info && infoPath info /= path
      then traverse order (infoPath info)
      else return [info]

-- this function should be clear enough
getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

-- this function first applies infoPerms to extract the Maybe Permissions of a Info
-- this is done by record syntax's accessors
-- Now Permissions also have record syntax, and so we use "searchable" to access its value
isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms
-- After some trying this is what i think "maybe" does
-- "maybe" takes in a default and a function, and finally a "Maybe a"
-- if the value in the "Maybe a" is Nothing, then return the default value
-- else, apply the function to the "Maybe a" and output its result

-- this is a combinator that turns an IO action that might throw an exception into one that wraps its result in Maybe
-- i.e. if "liftM act" throw exception, output "IO (Nothing)"
-- else "IO (Just `liftM` act)"
maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle (\(e::SomeException) -> return Nothing) (Just `liftM` act)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

-- traverse as defined above is pretty dense. here's the simpler and easier to understand one
traverseVerbose order path = do
    names <- getDirectoryContents path
    let userNames = filter (`notElem` [".", ".."]) names
    contents <- mapM getEntryName ("" : names)
    recursiveContents <- mapM recurse (order contents)
    return (concat recursiveContents)
  where getEntryName name = getInfo (path </> name)
        isDirectory info = case infoPerms info of
                             Nothing -> False
                             Just perms -> searchable perms
        recurse info = do
          if isDirectory info && infoPath info /= path
            then traverseVerbose order (infoPath info)
            else return [info]
