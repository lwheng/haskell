import System.IO
import Data.Char(toUpper)

main :: IO ()
main = do
        -- openFile :: FilePath -> IOMode -> IO Handle
        -- openFile returns a "IO Handle"
        -- so we use "<-" to extract the "Handle" out of "IO Handle"
        inh <- openFile "input.txt" ReadMode
        outh <- openFile "output.txt" WriteMode
        mainloop inh outh
        hClose inh
        hClose outh
        -- always safer to hClose after we are done

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh =
  do
    -- hIsEOF :: Handle -> IO Bool
    -- it returns IO Bool, so we use <- again to get the Bool
    ineof <- hIsEOF inh
    if ineof
      -- if it is the EOF of the input, we "return" an empty tuple
      -- "return" wraps its argument into an IO action
      -- so we get: IO ()
      then return ()
      else do
            -- hGetLine :: Handle -> IO String
            inputStr <- hGetLine inh
            -- hPutStrLn :: Handle -> String -> IO ()
            hPutStrLn outh (map toUpper inputStr)
            mainloop inh outh
            -- call self again to loop
