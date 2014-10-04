a `plus` b = a + b

data a `pair` b = a `Pair` b
  deriving (Show)

foo = Pair 1 2
bar = True `Pair` "quux"

-- Just a simple demo of how functions in Haskell can be used both prefix and infix
-- In fact, functions can also be defined infix
-- So why infix?
-- Main advantage is readability
-- E.g.
-- 3 `elem` [1,2,3,4] instead of 
-- elem 3 [1,2,3,4]
--
-- E.g.
-- "foo" `IsPrefixOf` "foobar"
--
-- Careful that backticks, `, are only used to wrap functions in Haskell, and NOT some expression that returns a function
