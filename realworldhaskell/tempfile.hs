import System.IO
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO.Error (catchIOError)
import Control.Exception (finally)

main :: IO ()
main = withTempFile "mytemp.txt" myAction

{- The guts of the program. Called with the path and handle a temporary file. When this function exits, that file wil lbe closed and deleted because myAction was called from withTempFile -}
myAction :: FilePath -> Handle -> IO ()
myAction tempname temph = do
  -- Start by displaying a greeting on the tutorial
  putStrLn $ "Welcome to the tempfile.hs"
  putStrLn $ "I have a temporary file at " ++ tempname

  -- Let's see what the initial position is
  pos <- hTell temph
  putStrLn $ "My initial position is " ++ show pos

  -- Now let's write something into the temporary file
  let tempdata = show [1..10]
  putStrLn $ "Writing one line containing " ++ show (length tempdata) ++ " bytes: " ++ tempdata
  hPutStrLn temph tempdata

  -- Get our new position
  pos <- hTell temph
  putStrLn $ "After writing, my new position is " ++ show pos

  -- Seek to the begining of the file and display it
  putStrLn $ "The file content is: "
  hSeek temph AbsoluteSeek 0

  -- hGetContents performs a lazy read of the entire file
  c <- hGetContents temph

  -- Copy the file byte-for-byte to stdout, followed by \n
  putStrLn c

  -- We display it as a Haskell literal too
  putStrLn $ "Which could be expressed as this Haskell literal: "
  print c


{-
 - This function takes 2 parameters: a filename pattern and another function. It creates a temporary file, and pass the name and Handle of that file to the given function
 -
 - The temporary file is created with openTempFile. The directory is the one indicated by getTemporaryDirectory, or, if the system has no notion of a temporary directory, "." is used. The given pattern is passed to openTempFile.
 -
 - After the given function terminates, even if it terminates due to an exception, the Handle is closed and the file is deleted
 -}

withTempFile :: String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile pattern func = do
  tempdir <- catchIOError (getTemporaryDirectory) (\_ -> return ".")
  -- catchIOError takes 2 functions: first to run, second to run when exception occurs
  (tempfile, temph) <- openTempFile tempdir pattern

  finally (func tempfile temph)
          (do
             hClose temph
             removeFile tempfile)
