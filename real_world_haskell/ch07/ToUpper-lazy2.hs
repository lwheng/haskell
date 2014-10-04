import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
        inh <- openFile "input.txt" ReadMode
        outh <- openFile "output.txt" WriteMode
        inputStr <- hGetContents inh
        let result = (map toUpper) inputStr
        hPutStr outh result
        hClose inh
        hClose outh
