module PodDB where

import Control.Monad          (when)
import Data.List              (sort)
import Database.HDBC
import Database.HDBC.Sqlite3
import PodTypes

connect
  :: FilePath
  -> IO Connection
connect path = do
  conn <- connectSqlite3 path
  prepDB conn
  return conn

prepDB
  :: IConnection conn
  => conn
  -> IO ()
prepDB conn = do
  tables <- getTables conn
  when ("podcasts" `notElem` tables) $ do
    run conn "CREATE TABLE podcasts (\
              \castid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
              \castURL TEXT NOT NULL UNIQUE)" []
    return ()
  when ("episodes" `notElem` tables) $ do
    run conn "CREATE TABLE episodes (\
              \epid INTEGER NOT NULL PRIMARY KEY AUTOINCREMENT,\
              \epcastid INTEGER NOT NULL,\
              \epurl TEXT NOT NULL,\
              \epdone INTEGER NOT NULL,\
              \UNIQUE(epcastid, epurl),\
              \UNIQUE(epcastid, epid))" []
    return ()
  commit conn

addPodcast
  :: IConnection conn
  => conn
  -> Podcast
  -> IO Podcast
addPodcast conn podcast = handleSql errorHandler $ do
  run conn "INSERT INTO podcasts (castURL) VALUES (?)" [toSql (castURL podcast)]

  r <- quickQuery' conn "SELECT castid FROM podcasts WHERE castURL = ?" [toSql (castURL podcast)]
  case r of
    [[x]] -> return $ podcast { castId = fromSql x }
    y     -> fail $ "addPodcast: unexpected result: " ++ show y
  where
    errorHandler e = do
      fail $ "Error adding podcast: does this URL already exists?\n" ++ show e

addEpisode
  :: IConnection conn
  => conn
  -> Episode
  -> IO ()
addEpisode conn ep = do
  run
    conn
    "INSERT OR IGNORE INTO episodes (epCastId, epURL, epDone) VALUES (?, ?, ?)"
    [toSql (castId . epCast $ ep), toSql (epURL ep), toSql (epDone ep)]
  return ()  

updateEpisode
  :: IConnection conn
  => conn
  -> Episode
  -> IO ()
updateEpisode conn episode = do
  run
    conn
    "UPDATE episodes SET epCastId = ?, epURL = ?, epDone = ? WHERE epId = ?"
    [toSql (castId . epCast $ episode), toSql (epURL episode), toSql (epDone episode), toSql (epId episode)]
  return ()

removePodcast
  :: IConnection conn
  => conn
  -> Podcast
  -> IO ()
removePodcast conn podcast = do
  run conn "DELETE FROM episodes WHERE epcastid = ?" [toSql (castId podcast)]
  run conn "DELETE FROM podcasts WHERE castid = ?" [toSql (castId podcast)]
  return ()

getPodcasts
  :: IConnection conn
  => conn
  -> IO [Podcast]
getPodcasts conn = do
  res <- quickQuery' conn "SELECT castid, casturl FROM podcasts ORDER BY castid" []
  return (map convPodcastRow res)

getPodcast
  :: IConnection conn
  => conn
  -> Integer
  -> IO (Maybe Podcast)
getPodcast conn wantedId = do
  res <- quickQuery' conn "SELECT castid, casturl FROM podcasts WHERE castid = ?" [toSql wantedId]
  case res of
    [x] -> return (Just (convPodcastRow x))
    []  -> return Nothing
    x   -> fail $ "Really bad error; more than one podcast with ID"

convPodcastRow
  :: [SqlValue]
  -> Podcast
convPodcastRow [svId, svURL] = Podcast {
                                         castId  = fromSql svId
                                       , castURL = fromSql svURL
                                       }
convPodcastRow x = error $ "Can't convert podcast row " ++ show x

getPodcastEpisodes
  :: IConnection conn
  => conn
  -> Podcast
  -> IO [Episode]
getPodcastEpisodes conn pc = do
  r <- quickQuery' conn "SELECT epId, epURL, epDone FROM episodes WHERE epCastId = ?" [toSql (castId pc)]
  return (map convEpisodeRow r)
  where
    convEpisodeRow [svId, svURL, svDone] = Episode {
                                                     epId   = fromSql svId
                                                   , epURL  = fromSql svURL
                                                   , epDone = fromSql svDone
                                                   , epCast = pc
                                                   }
