import Database.HDBC.Sqlite3 (connectSqlite3, Connection)
import Database.HDBC
import System.Environment

main :: IO ()
main = do
  [filePath, maxID] <- getArgs
  conn <- connectSqlite3 filePath
  query conn (read maxID) 
  disconnect conn

query
  :: Connection
  -> Int
  -> IO ()
query conn maxID = do
  -- Run query and store in r
  r <- quickQuery' conn
         "SELECT id, desc FROM test WHERE id <= ? ORDER BY id, desc"
         [toSql maxID]

  -- Convert each row into a String
  let
    stringRows = map convRow r

  -- Print the rows out
  mapM_ putStrLn stringRows
  where
    convRow :: [SqlValue] -> String
    convRow [sqID, sqDesc] = show intID ++ ": " ++ desc
      where
        intID = (fromSql sqID) :: Integer
        desc  = case fromSql sqDesc of
                  Just x  -> x
                  Nothing -> "NULL"
    convRow x = fail $ "Unexpected result: " ++ show x
