module PerfectBetween (
    perfectBetween
) where

perfectBetween :: Integer -> Integer -> [Integer]
perfectBetween a b = filter (>a) squares
  where
    two = 2 :: Integer
    squares = takeWhile (<b) $ map (^two) [1..]
