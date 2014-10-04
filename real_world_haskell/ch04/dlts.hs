-- this file reads test.c and extracts the 2nd word in each line
-- e.g. DLT_...
-- again this haskell function demos currying, chaining functions

import Data.List (isPrefixOf)

dlts :: String -> [String]

dlts = foldr step [] . lines
  where step l ds
          | "#define DLT_" `isPrefixOf` l = secondWord l : ds
          | otherwise
        secondWord = head . tail . words
