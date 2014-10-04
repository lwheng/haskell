suffixes :: [a] -> [[a]]
suffixes xs@(_:xs') = xs : suffixes xs'
suffixes _ = []

-- As-pattern
-- <left>@<right>
-- if the left matches the right, left will be bounded to the expression on the right
-- can avoid a little allocation


-- Function composition
-- i.e. f(g(x))

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- Fortunately Haskell is like math
-- tails returns all the suffixes of a list, including an empty list
-- we use init to exclude the empty one
suffixes5 = init . tails
