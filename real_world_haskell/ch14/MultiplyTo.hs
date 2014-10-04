guarded :: Bool -> [a] -> [a]
guarded True  xs = xs
guarded False _  = []


-- This puts into practice the nested loop we learned earlier
-- This is a simple brute force constraint solver
-- Given an integer, find all pairs of factors
multiplyTo :: Int -> [(Int, Int)]
multiplyTo n = do
  x <- [1..n]
  y <- [x..n]
  guarded (x * y == n) $
    return (x, y)
