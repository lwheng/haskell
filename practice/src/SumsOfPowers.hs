module SumsOfPowers (
    sumsOfPower
) where

import GHC.Float (int2Double)
import qualified Data.Set as Set

sumsOfPower
  :: Int
  -> Int
  -> Int
sumsOfPower x n = Set.size sumsToX
  where
    largest :: Int
    largest = floor $ (int2Double x) ** (int2Double 1 / int2Double n)

    scope = map (^n) [1..largest]
    set   = Set.fromList scope
    sets  = Set.powerSet set
    sumsToX = Set.filter (\s -> x == sum (Set.elems s)) sets
