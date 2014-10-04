-- Consider a binary tree type
data Tree a = Node (Tree a) (Tree a)
            | Leaf a
            deriving (Show)

-- if we want a tree of strings to be converted to a tree of lengths of strings,
-- we may consider:
treeLengths (Leaf s) = Leaf (length s)
treeLengths (Node l r) = Node (treeLengths l) (treeLengths r)

-- in fact, in general:
treeMap :: (a -> b) -> Tree a -> Tree b
-- treeMap, takes a function and a Tree of type "a" and converts to a Tree of type "b"
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)
-- this way, treeLengths can be defined using treeMap and "length" functions

instance Functor Tree where
    fmap = treeMap
-- fmap is kinda like a lifting function
-- fmap takes a function over oridinary values (a -> b)
-- and lifts it to become a function over containers (f a -> f b)
-- in this case, "f" is Tree
-- effectively treeMap is already a lifting function
-- we could write:
{-
 - instance Functor Tree where
 -   fmap f (Leaf a) = Leaf (f a)
 -   fmap f (Node l r) = Node (fmap f l) (fmap f r)
-}
--
-- So what is the difference between "map" and "fmap" ?
-- map is only for Lists. fmap is for any container
-- In fact, map can be defined using fmap, where the container is List
-- that's why we can do this:
{-
instance Functor [] where
    fmap = map
-}
-- and that's why these are the same
-- fmap length ["foo", bar"]
-- map length ["foo", "bar"]
--

{-
instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f (Just x) = Just (f x)
-}

-- Just need to know that <$> is an alias for fmap
-- Just need to import Control.Applicative
