import System.IO
import System.IO.Error(catchIOError)
import System.Directory(getTemporaryDirectory, removeFile)
import Control.Exception(finally)

-- all IO actions begin with main
main :: IO ()
main = withTempFile "mytemp.txt" myAction

myAction :: FilePath -> Handle -> IO ()
myAction tempName temph =
  do
    putStrLn "Welcome to TempFile.hs"
    putStrLn $ "I  have a temporary file at " ++ tempName
    -- recall that $ just wraps its RHS with brackets

    -- Let's see what the initial position is
    -- hTell :: Handle -> IO Integer
    -- hTell returns the current seek position in the file Handle
    pos <- hTell temph
    putStrLn $ "My initial position is " ++ show pos

    -- Now, write something to the temp file
    let tempdata = show [1..10]
    putStrLn $ "Writing one line containing " ++
               show (length tempdata) ++ " bytes: " ++
               tempdata
    hPutStrLn temph tempdata

    -- Get our new position
    pos <- hTell temph
    -- Note that this does not mean "pos" is modified in memory
    -- It means "pos" is pointing to a different value
    putStrLn $ "After writing, my new position is " ++ show pos

    -- Seek to start of file and display it
    putStrLn $ "The file content is: "
    -- hSeek :: Handle -> SeekMode -> Integer -> IO ()
    hSeek temph AbsoluteSeek 0

    -- hGetContents :: Handle -> IO String
    -- hGetContents is a lazy read
    c <- hGetContents temph

    putStrLn c
    -- note that putStrLn always add a "\n" at the end
    -- That's why we have another empty line
    
    -- Display "c" as Haskell literal
    putStrLn $ "Which could be expressed as this Haskell literal: "
    print c

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = 
  do
    tempdir <- catchIOError (getTemporaryDirectory) (\_ -> return ".")
    -- this is how we use "catchIOError"
    -- the first function, getTemporaryDirectory will run
    -- if there's an exception, the 2nd one will run

    -- openTempFile :: FilePath -> String -> IO (FilePath, Handle)
    -- openTempFile will open a temp file using the FilePath and String (Pattern)
    (tempfile, temph) <- openTempFile tempdir pattern

    -- Now we have the FilePath and Handle, we are ready to call our function
    -- we use "finally"
    -- finally takes two IO actions, regardless whether the first action causes an exception,
    -- the second one will be executed
    -- in this case, we want our tempfile to be deleted no matter what
    finally (func tempfile temph)
            (do hClose temph
                removeFile tempfile)
