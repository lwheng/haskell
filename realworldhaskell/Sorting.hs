module Sorting where

import Control.Parallel (par, pseq)

parSort
  :: (Ord a)
  => [a]
  -> [a]
parSort []     = []
parSort (x:xs) = force greater `par` (force lesser `pseq` (lesser ++ x:greater))
  where
    lesser  = sort [y | y <- xs, y <  x]
    greater = sort [y | y <- xs, y >= x]

sillySort
  :: (Ord a)
  => [a]
  -> [a]
sillySort []     = []
sillySort (x:xs) = greater `par` (lesser `pseq` (lesser ++ x:greater))
  where
    lesser  = sort [y | y <- xs, y <  x]
    greater = sort [y | y <- xs, y >= x]

sort
  :: (Ord a)
  => [a]
  -> [a]
sort []     = []
sort (x:xs) = lesser ++ x:greater
  where
    lesser  = sort [y | y <- xs, y <  x]
    greater = sort [y | y <- xs, y >= x]

force
  :: [a]
  -> ()
force xs = go xs `pseq` ()
  where
    go (_:xs) = go xs
    go []     = 1
