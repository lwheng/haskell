module Main where

import           Control.Monad (forM_)
import           Control.Parallel (pseq)
import           Control.Parallel.Strategies (NFData(..), rseq, Strategy, parMap, using)
import           Data.List (foldl', sortBy)
import           LineChunks (chunkedReadWith)
import           System.Environment (getArgs)
import           Text.Regex.PCRE.Light (compile, match)
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

main :: IO ()
main = return ()

countURLs :: [L.ByteString] -> M.Map S.ByteString Int
countURLs = mapReduce rseq (foldl' augment M.empty . L.lines)
                      rseq (M.unionsWith (+))
  where
    augment map line = case match (compile pattern []) (strict line) [] of
                         Just (_:url:_) -> M.insertWith' (+) url 1 map
                         _              -> map
    strict  = S.concat . L.toChunks
    pattern = S.pack "\"(?:GET|POST|HEAD) ([^ ]+) HTTP/"

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
