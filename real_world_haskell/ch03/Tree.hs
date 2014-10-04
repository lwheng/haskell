-- A recursive type definition
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show)

simpleTree = Node "root" (Node "left child" Empty Empty) (Node "right child" Empty Empty)

-- Exercise: To remove the need for the second constructor
-- Use Maybe
-- Took me a while to come up with this definition
-- Just gotta let it sink in a little
data Tree' a = Node' (Maybe a) (Maybe (Tree' a)) (Maybe (Tree' a))
  deriving (Show)

maybeTree = Node' (Just "parent") (Just (Node' (Just "left") Nothing Nothing)) Nothing


heightOfTree :: (Tree a) -> Int
-- Always easy to start with the base case
heightOfTree Empty = 0
heightOfTree (Node _ Empty Empty) = 1 -- in fact we can remove this line
--heightOfTree (Node a left right) = let
--                                    b = heightOfTree left
--                                    c = heightOfTree right
--                                   in
--                                    if b > c
--                                    then 1 + b
--                                    else 1 + c
-- the previous line of the heightOfTree was my first attempt. totally forgot there's a max function
heightOfTree (Node a left right) = 1 + (max (heightOfTree left) (heightOfTree right))
