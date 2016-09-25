import System.Environment
import Text.Printf

main = do
  [d] <- map read `fmap` getArgs
  printf "%f\n" (mean2 [1..d])

mean
  :: [Double]
  -> Double
mean xs = sum xs / fromIntegral (length xs)

mean2
  :: [Double]
  -> Double
mean2 xs = {-# SCC "mean2" #-} sum xs / fromIntegral (length xs)
