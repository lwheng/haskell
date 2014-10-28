import System.IO
import Data.Char (toUpper)

main :: IO ()
main = do
  inH <- openFile "input.txt" ReadMode
  outH <- openFile "output.txt" WriteMode
  inputStr <- hGetContents inH
  let result = processData inputStr
  hPutStr outH result
  hClose inH
  hClose outH

processData :: String -> String
processData = map toUpper

main2 :: IO ()
main2 = do
  inH <- openFile "input.txt" ReadMode
  outH <- openFile "output.txt" WriteMode
  inputStr <- hGetContents inH
  hPutStr outH (map toUpper inputStr)
  hClose inH
  hClose outH

main3 :: IO ()
main3 = do
  inputStr <- readFile "input.txt"
  writeFile "output.txt" (map toUpper inputStr)

main4 :: IO ()
main4 = interact (map toUpper)
