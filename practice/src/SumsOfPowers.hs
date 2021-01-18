module SumsOfPowers (
    main
) where

import GHC.Float (int2Double)
import qualified Data.Set as Set

main :: IO ()
main = do
  [x, n] <- map read . lines <$> getContents :: IO [Int]

  let
    largest :: Int
    largest = floor $ (int2Double x) ** (int2Double 1 / int2Double n)

    scope = map (^n) [1..largest]
    set   = Set.fromList scope
    sets  = Set.powerSet set
    sumsToX = Set.filter (\s -> x == sum (Set.elems s)) sets

  print $ Set.size sumsToX
