module PascalTriangle (
    main
) where

import Control.Monad (forM_)

main :: IO ()
main = do
  k <- (read <$> getLine) :: IO Integer
  forM_ [1..k] $ \row -> putStrLn $ unwords $ map show $ pascalRow row

factorial
  :: Integer
  -> Integer
factorial n = product [1..n]

pascalValue
  :: Integer
  -> Integer
  -> Integer
pascalValue n r = div top bot
  where
    top = factorial n
    bot = left * right
    left = factorial r
    right = factorial (n - r)

pascalRow
  :: Integer
  -> [Integer]
pascalRow row = map (pascalValue (row - 1)) cols
  where
    cols = [0..row-1]
