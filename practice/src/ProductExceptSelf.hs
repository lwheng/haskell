module ProductExceptSelf (
    main
) where

import Data.List

main :: IO ()
main = do
  print $ productExceptSelf [1,2,3,4]


productExceptSelf :: [Int] -> [Int]
productExceptSelf xs = map f xs
  where
    f :: Int -> Int
    f x = product $ xs \\ [x]
