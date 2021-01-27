module LongestLeadingFragment (
    longestLeadingFragment
) where

import qualified Data.HashMap.Strict as Map

longestLeadingFragment
  :: Int
  -> Int
  -> [Int]
  -> [Int]
longestLeadingFragment x y nums = take index nums
  where
    indexed = zip [1..] nums

    checker = checkEqualCount x y

    alterFunc :: Maybe Int -> Maybe Int
    alterFunc Nothing = Just 1
    alterFunc (Just c) = Just (c + 1)

    folded = foldl (\(m,j) (i,n) ->
                      let
                        m1 = Map.alter alterFunc n m
                        i1 = if checker m1
                               then i
                               else j
                      in
                        (m1, i1)
                        
                   ) (Map.empty, 0) indexed
    (_, index) = folded

checkEqualCount
  :: Int
  -> Int
  -> Map.HashMap Int Int
  -> Bool
checkEqualCount x y m =
  case (Map.lookup x m, Map.lookup y m) of
    (Just countX, Just countY) -> countX == countY
    _                          -> False
