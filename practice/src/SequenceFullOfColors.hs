module SequenceFullOfColors (
  main
) where

import Control.Monad (forM_)

main :: IO ()
main = do
  testCases <- tail . lines <$> getContents

  let
    result = map checkTestCase testCases
  forM_ result print

checkTestCase
  :: String
  -> Bool
checkTestCase testCase = result && r == g && y == b
  where
    (r, g, y, b, result) = foldl consumeChar (0, 0, 0, 0, True) testCase

consumeChar
  :: (Int, Int, Int, Int, Bool)
  -> Char
  -> (Int, Int, Int, Int, Bool)
consumeChar (r, g, y, b, valid) c = check consumed
  where
    consumed = 
      case c of
        'R' -> (r + 1, g    , y    , b    , valid)
        'G' -> (r    , g + 1, y    , b    , valid)
        'Y' -> (r    , g    , y + 1, b    , valid)
        'B' -> (r    , g    , y    , b + 1, valid)
        _   -> error "Unsupported color"

check
  :: (Int, Int, Int, Int, Bool)
  -> (Int, Int, Int, Int, Bool)
check (r, g, y, b, valid) =
  if (abs (r - g) > 1 || abs (y - b) > 1)
    then
      (r, g, y, b, False)
    else
      (r, g, y, b, valid)
