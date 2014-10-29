import Control.Monad (filterM, forM, liftM)
import System.Directory (Permissions(..), getModificationTime, getPermissions, getDirectoryContents)
import Data.Time.Clock (UTCTime(..))
import System.FilePath (takeExtension, (</>), takeFileName)
import Control.Exception (bracket, handle)
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import Data.Char (toLower)

import RecursiveContents (getRecursiveContents)

data Info = Info {
  infoPath :: FilePath,
  infoPerms :: Maybe Permissions,
  infoSize :: Maybe Integer,
  infoModTime :: Maybe UTCTime
} deriving (Eq, Ord, Show)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
  names <- getUsefulContents path
  contents <- mapM getInfo (path : map (path </>) names)
  liftM concat $ forM (order contents) $ \info -> do
    if isDirectory info && infoPath info /= path
    then traverse order (infoPath info)
    else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing) :: IOError -> IO (Maybe a)) (Just `liftM` act)


getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)

data Iterate seed = Done {unwrap :: seed}
                  | Skip {unwrap :: seed}
                  | Continue {unwrap :: seed}
                    deriving (Show)

type Iterator seed = seed -> Info -> Iterate seed

foldTree :: Iterator a -> a -> FilePath -> IO a
foldTree iter initSeed path = do
  endSeed <- fold initSeed path
  return (unwrap endSeed)
  where
    fold seed subpath = getUsefulContents subpath >>= walk seed
    walk seed (name : names) = do
      let path' = path </> name
      info <- getInfo path'
      case iter seed info of
        done@(Done _) -> return done
        Skip seed' -> walk seed' names
        Continue seed'
          | isDirectory info -> do
              next <- fold seed' path'
              case next of
                done@(Done _) -> return done
                seed'' -> walk (unwrap seed'') names
          | otherwise -> walk seed' names
    walk seed _ = return (Continue seed)

atMostThreePictures :: Iterator [FilePath]
atMostThreePictures paths info
  | length paths == 3 = Done paths
  | isDirectory info && takeFileName path == ".svn" = Skip paths
  | extension `elem` [".jpg", ".png"] = Continue (path : paths)
  | otherwise = Continue paths
  where extension = map toLower (takeExtension path)
        path = infoPath info

countDirectories count info =
  Continue (if isDirectory info
            then count + 1
            else count)
