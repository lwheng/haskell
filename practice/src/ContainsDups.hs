module ContainsDups (
    main
  , containsDups
) where

import qualified Data.HashMap.Strict as Map

main :: IO ()
main = print $ containsDups [1,2,3,1]


containsDups :: [Int] -> Bool
containsDups xs =
  let
    maybePlus :: Maybe Int -> Maybe Int
    maybePlus Nothing = Just 1
    maybePlus (Just v) = Just (v + 1)
    m = foldr (Map.alter maybePlus) emptyMap xs
  in
    length (Map.keys m) /= length xs
  where
    emptyMap = Map.empty :: Map.HashMap Int Int
