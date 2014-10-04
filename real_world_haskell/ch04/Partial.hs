import Data.List

isInAny needle haystack = any inSequence haystack
  where inSequence s = needle `isInfixOf` s

isInAny2 needle haystack = any (\s -> needle `isInfixOf` s) haystack
-- demonstrating anonymous functions

isInAny3 needle haystack = any (isInfixOf needle) haystack
-- demo currying of functions


isInAny4 needle haystack = any (needle `isInfixOf`) haystack
-- demo Sections i.e. providing the left/right argument to the partial function
