import Data.Char(toUpper)

main :: IO ()
main = do
        -- Using readFile and writeFile means we don't even need to deal with the Handle ourselves. No need to use hGetContents, hPutStr or even close the Handle with hClose. readFile and writeFile do that internally
        -- An advantage of dealing with Handles ourselves might be we can specify the IOMode: ReadMode, WriteMode, ReadWriteMode, AppendMode

        -- readFile :: FilePath -> IO String
        inputStr <- readFile "input.txt"
        -- writeFile :: FilePath -> String -> IO ()
        writeFile "output.txt" (map toUpper inputStr)
