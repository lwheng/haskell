-- Haskell is smart enough to help derives the most common functions
-- There's a limit to what it can do 
-- for e.g. it cannot derive for functions

data Color = Red | Green | Blue
  deriving (Show, Eq, Read, Ord)
