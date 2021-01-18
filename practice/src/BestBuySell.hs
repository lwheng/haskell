module BestBuySell (
  bestBuySell
) where

-- Say you have an array for which the ith element is the price of a given stock on day i.
bestBuySell :: [Int] -> [(Int, Int, Int)]
bestBuySell prices = [(buy, sell, sell - buy) | b <- r, s <- r, b < s, buy <- [prices !! b], sell <- [prices !! s], sell - buy > 0]
  where
    r = [0 .. length prices - 1]
