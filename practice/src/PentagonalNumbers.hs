module PentagonalNumbers (
    pentagonalNumbers
) where

pointsOnPentagon
  :: Int
  -> Int
pointsOnPentagon n = total - overlap
  where
    total   = (n - 1) * 5
    overlap = (n `safeSubtract` 1) + (n `safeSubtract` 2)

safeSubtract
  :: Int
  -> Int
  -> Int
safeSubtract x y = if res < 0
                     then 0
                     else res
  where
    res = x - y

pentagonalNumbers
  :: Int
  -> Int
pentagonalNumbers 1 = 1
pentagonalNumbers n = pointsOnPentagon n + pentagonalNumbers (n - 1)
