module NextGreaterElement (
  nextGreaterElement
) where

import qualified Data.List as List

nextGreaterElement
  :: [Int]
  -> [(Int, Maybe Int)]
nextGreaterElement xs = foldl f [] $ List.tails xs
  where
    maybeMax _ [] = Nothing
    maybeMax l ms = case filter (>l) ms of
                      [] -> Nothing
                      mm -> Just $ head $ List.sort mm

    f acc []     = acc
    f acc (l:ls) = (l, maybeMax l ls) : acc
