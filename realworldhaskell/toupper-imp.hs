import System.IO
import Data.Char (toUpper)

main :: IO ()
main = do
  inHandle <- openFile "input.txt" ReadMode
  outHandle <- openFile "output.txt" WriteMode
  mainLoop inHandle outHandle
  hClose inHandle
  hClose outHandle

mainLoop :: Handle -> Handle -> IO ()
mainLoop inH outH = do
    ineof <- hIsEOF inH
    if ineof
      then return ()
      else do
        inputStr <- hGetLine inH
        hPutStrLn outH (map toUpper inputStr)
        mainLoop inH outH
