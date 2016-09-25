module Main where

import           Control.Monad (forM_)
import           Control.Parallel.Strategies
import           Control.Parallel (pseq, par)
import           Data.Int (Int64)
import           System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LB

import LineChunks (chunkedReadWith)

lineCount :: [LB.ByteString] -> Int64
lineCount = mapReduce rdeepseq (LB.count '\n')
                      rdeepseq sum

main :: IO ()
main = do
  args <- getArgs
  forM_ args $ \path -> do
    numLines <- chunkedReadWith lineCount path
    putStrLn $ path ++ ": " ++ show numLines

mapReduce
  :: Strategy b -- evaluation strategy for mapping
  -> (a -> b)   -- map function
  -> Strategy c -- evaluation strategy for reduction
  -> ([b] -> c) -- reduce function
  -> [a]        -- list to map over
  -> c
mapReduce mapStrat mapFunc reduceStrat reduceFunc input = mapResult `pseq` reduceResult
  where
    mapResult    = parMap mapStrat mapFunc input
    reduceResult = reduceFunc mapResult `using` reduceStrat
