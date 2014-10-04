data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

-- Here we use guards, |, which is like a IF.
-- IF (condition) THEN return something
-- kinda like CASE too, except that in CASE we use -> instead of =
-- See Lending.hs for more guards
nodesAreSame (Node a _ _) (Node b _ _)
  | a == b = Just a
nodesAreSame _ _ = Nothing

-- cannot put 'a' in 2 locations and think that it would mean they are equal
bad_nodesAreSame (Node a _ _) (Node a _ _) = Just a
bad_nodesAreSame _            _            = Nothing
