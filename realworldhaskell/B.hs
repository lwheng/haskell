{-# LANGUAGE BangPatterns #-}

import Data.List
import System.Environment
import Text.Printf

main = do
  [d] <- map read `fmap` getArgs
  printf "%f\n" (mean' [1..d])

mean
  :: [Double]
  -> Double
mean xs = s / fromIntegral n
  where
    (n, s)     = foldl k (0, 0) xs
    k (n, s) x = (n+1, s+x)

mean'
  :: [Double]
  -> Double
mean' xs = s / fromIntegral n
  where
    (n, s)     = foldl' k (0, 0) xs
    k (n, s) x = (n+1, s+x)

mean''
  :: [Double]
  -> Double
mean'' xs = s / fromIntegral n
  where
    (n, s)     = foldl' k (0, 0) xs
    k (n, s) x = n `seq` x `seq` (n+1, s+x)

meanBang
  :: [Double]
  -> Double
meanBang xs = s / fromIntegral n
  where
    (n, s)       = foldl k (0, 0) xs
    k (!n, !s) x = (n+1, s+x)
