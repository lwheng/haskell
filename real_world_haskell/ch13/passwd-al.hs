import Data.List
import System.IO
import Control.Monad (when)
import System.Exit
import System.Environment (getArgs)


-- To compile to an executable
-- ghc -o passwd-al passwd-al.hs
main = do
    -- Load the command line arguments
    args <- getArgs

    -- If we don't have the right number of arguments, give an error and abort
    when (length args /= 2) $ do
       putStrLn "Syntax: password-al filename uid"
       exitFailure

    -- Read the file lazily
    content <- readFile (args !! 0)

    -- Compute the username in pure code
    let username = findByUID content (read (args !! 1))

    -- Display the result
    case username of
        Just x -> putStrLn x
        Nothing -> putStrLn "Could not find that UID"

-- Given the entire input and a UID, see if we can a username
findByUID :: String -> Integer -> Maybe String
findByUID content uid = 
    let al = map parseline . lines $ content
        in lookup uid al

parseline :: String -> (Integer, String)
parseline input = 
    let fields = split ':' input
        in (read (fields !! 2), fields !! 0)

split :: Eq a => a -> [a] -> [[a]]
split _ [] = [[]]
split delim str = 
    -- span is a function that splits the second into a 2-tuple
    -- e.g. span (/= 'f') "abcdefg" -----> ("abcde", "fg")
    let (before, remainder) = span (/= delim) str
    in
        before : case remainder of
                     [] -> [] -- if there's nothing else, then return
                     x -> split delim (tail x) -- if there's more, we recurse (excluding the delim character)
