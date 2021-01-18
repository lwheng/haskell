module PerfectBetween (
    main
) where

main :: IO ()
main = print $ perfectBetween 25 60


perfectBetween :: Int -> Int -> [Int]
perfectBetween a b = filter (>a) squares
  where
    squares = takeWhile (<b) $ map (^2) [1..]
