module SortMain where

import Data.Time.Clock    (diffUTCTime, getCurrentTime)
import System.Environment (getArgs)
import System.Random      (StdGen, getStdGen, randoms)

import Sorting

-- testFunction = sort
-- testFunction = seqSort
testFunction = parSort
-- testFunction = parSort2 2

randomInts
  :: Int
  -> StdGen
  -> [Int]
randomInts k g = force result `seq` result
  where
    result = take k (randoms g)

main = do
  args <- getArgs
  let
    count | null args = 500000
          | otherwise = read (head args)
  input <- randomInts count `fmap` getStdGen
  putStrLn $ "We have " ++ show (length input) ++ " elements to sort."

  start <- getCurrentTime
  let
    sorted = testFunction input
  putStrLn $ "Sorted all " ++ show (length sorted) ++ " elements."

  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
