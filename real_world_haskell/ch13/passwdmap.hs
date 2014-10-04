import Data.List
import qualified Data.Map as Map
import System.IO
import Text.Printf (printf)
import System.Environment (getArgs)
import System.Exit
import Control.Monad (when)

{- data type to store an entry in /etc/passwd -}
data PasswdEntry = PasswdEntry {
  userName :: String,
  password :: String,
  uid :: Integer,
  gid :: Integer,
  gecos :: String,
  homeDir :: String,
  shell :: String }
  deriving (Eq, Ord)

{- Writing our own instance of Show to print out the PasswdEntry -}
instance Show PasswdEntry where
    show pe = printf "%s:%s:%d:%d:%s:%s:%s"
              (userName pe) (password pe) (uid pe) (gid pe) (gecos pe)
              (homeDir pe) (shell pe)

{- Writing an instance of Read to read data into a PasswdEntry -}
instance Read PasswdEntry where
    readsPrec _ value = 
        case split ':' value of
             [f1, f2, f3, f4, f5, f6, f7] ->
                 -- Generate a PasswdEntry
                 [(PasswdEntry f1 f2 (read f3) (read f4) f5 f6 f7, [])]
             x -> error $ "Invalid number of fields input: " ++ show x
        where
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

{- Convenience aliases -}
type UIDMap = Map.Map Integer PasswdEntry
type UserMap = Map.Map String PasswdEntry

{- Converts input data into maps -}
inputToMaps :: String -> (UIDMap, UserMap)
inputToMaps inp = (uidmap, usermap)
    where
    uidmap = Map.fromList . map (\pe -> (uid pe, pe)) $ entries
    usermap = Map.fromList . map (\pe -> (userName pe, pe)) $ entries
    entries = map read (lines inp)

main = do
    -- Load the command line arguments
    args <- getArgs

    -- If we don't have the right number of arguments, give an error and abort
    when (length args /= 1) $ do
       putStrLn "Syntax: passwdmap filename"
       exitFailure

    -- Read the file lazily
    content <- readFile (head args)
    let maps = inputToMaps content
    mainMenu maps

mainMenu maps@(uidmap, usermap) = do
    putStr optionText
    hFlush stdout
    sel <- getLine

    case sel of
        "1" -> lookupUserName >> mainMenu maps
        "2" -> lookupUID >> mainMenu maps
        "3" -> displayFile >> mainMenu maps
        "4" -> return ()
        _   -> putStrLn "Invalid selection" >> mainMenu maps

    where
    lookupUserName = do
        putStrLn "Username: "
        username <- getLine
        case Map.lookup username usermap of
            Nothing -> putStrLn "Username not found."
            Just x -> print x
    lookupUID = do
        putStrLn "UID: "
        uidstring <- getLine
        case Map.lookup (read uidstring) uidmap of
            Nothing -> putStrLn "UID not found."
            Just x -> print x
    displayFile =
        putStrLn . unlines . map (show . snd) . Map.toList $ uidmap
    optionText = 
        "\npasswdmap options:\n\
        \\n\
        \1    Look up a user name\n\
        \2    Look up a UID\n\
        \3    Display entire file\n\
        \4    Quit\n\n\
        \Your selection: "
