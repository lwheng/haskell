import Data.Char(toUpper)

isGreen :: IO Bool
isGreen = do
            putStrLn "Is green your favourite color?"
            inputStr <- getLine
            -- recall that "<-" means extract the pure value from the IO action
            -- "return" wraps its argument into an IO action
            -- "return" does not mean return the value to its caller
            return ((toUpper . head $ inputStr) == 'Y')

isYes :: String -> Bool
isYes input = (toUpper . head $ input) == 'Y'

isGreen2 = do
            putStrLn "Is green your favourite color?"
            inputStr <- getLine
            return (isYes inputStr)

returnTest :: IO ()
returnTest = do
              one <- return 1
              let two = 2
              putStrLn $ show (one + two)
              -- over here we demo that "return" does not mean returning value to its caller
              -- the last line in the "do" block is the return value
