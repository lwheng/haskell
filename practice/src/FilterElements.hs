module FilterElements (
  main
) where

import           Control.Monad (forM, forM_)
import qualified Data.List as List
--import qualified Data.HashMap.Strict as Map
import qualified Control.Parallel.Strategies as Par

main :: IO ()
main = do
  numOfTestCases <- read <$> getLine :: IO Int
  testCases <- forM [1..numOfTestCases] $ \_ -> do
    k <- (read . last . words) <$> getLine :: IO Int
    xs <- (map read . words) <$> getLine :: IO [Int]
    return (k, xs)

  let
    strat = Par.rseq
    results = Par.parMap strat handleTestCase testCases
  forM_ results printResult
  
printResult
  :: [Int]
  -> IO ()
printResult [] = putStrLn "-1"
printResult xs = putStrLn $ unwords $ map show xs

handleTestCase
  :: (Int, [Int])
  -> [Int]
handleTestCase (k, numbers) =
  let
    order = List.nub numbers
    validss = map head $ filter (\g -> length g >= k) $ List.group $ List.sort numbers
    valids = List.intersect order validss
  in
    valids
