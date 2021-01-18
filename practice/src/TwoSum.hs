module TwoSum (
  twoSum
) where

twoSum :: [Int] -> Int -> [(Int, Int)]
twoSum nums target = [(x, y) | x <- r, y <- r, x < y && nums !! x + nums !! y == target]
  where
    r = [0 .. length nums - 1]
