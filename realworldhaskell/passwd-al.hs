import Control.Monad (when)
import Data.List
import System.Environment (getArgs)
import System.Exit
import System.IO

main = do
  -- Load the command-line arguments
  args <- getArgs

  -- If we don't have the right number of arguments, give and error and abort
  case args of
    [filename, uid] -> do
                         -- Read the file
                         content <- readFile filename

                         -- Compute the username in pure code
                         let
                           username = findByUID content (read uid)

                         -- Display the result
                         case username of
                           Nothing -> putStrLn "Could not find the UID"
                           Just u  -> putStrLn u
    _               -> do
                         putStrLn "Syntax: passwd-dl filename uid"
                         exitFailure

-- Given the entire input and a UID, see if we can find a username.
findByUID :: String -> Integer -> Maybe String
findByUID content uid = let
                          al = map parseline . lines $ content
                        in
                          lookup uid al

-- Convert a colon-separated line into fields
parseline :: String -> (Integer, String)
parseline input = let
                    fields = split ':' input
                  in
                    (read (fields !! 2), fields !! 0)

-- | Takes a delimiter and a list.  Break up the list based on the delimiter
split :: Eq a => a -> [a] -> [[a]]
-- If the input is empty, the result is a list of empty lists.
split _ [] = [[]]
split delim str =
  -- Find the part of the list before delim and put it in "before".
  -- The rest of the list, including the leading delim, goes
  -- in "remainder".
  let
    (before, remainder) = span (/= delim) str
  in
    before : case remainder of
               [] -> []
               x -> -- If there is more data to process,
                    -- call split recursively to process it
                    split delim (tail x)
