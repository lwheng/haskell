import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
        -- two handles, one to read, one to write
        inh <- openFile "input.txt" ReadMode
        outh <- openFile "output.txt" WriteMode
        -- read lazily with hGetContents
        inputStr <- hGetContents inh
        -- apply our function to the data
        let result = processData inputStr -- compiler will only free the memory of inputStr AFTER it was used
                                          -- if we had decided to continue referencing inputStr, the memory cannot be freed
        -- write results to the output handle
        hPutStr outh result
        hClose inh
        hClose outh

-- a Pure function that doesn't deal with any IO
processData :: String -> String
processData = map toUpper
