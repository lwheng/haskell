import Data.Char (toUpper)

isGreen :: IO Bool
isGreen =
  putStrLn "Is green your favourite color?" >>
  getLine >>=
  (\inpStr -> return ((toUpper . head $ inpStr) == 'Y'))

isYes :: String -> Bool
isYes inpStr = (toUpper . head $ inpStr) == 'Y'

isGreen2 :: IO Bool
isGreen2 = do
  putStrLn "Is green your favourite color?"
  inpStr <- getLine
  return (isYes inpStr)

returnTest :: IO ()
returnTest = do
  one <- return 1
  let two = 2
  putStrLn $ show (one + two)
