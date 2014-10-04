module DList
    (
      DList
    , fromList
    , toList
    , empty
    , append
    , cons
    , dfoldr
    ) where

import Data.Monoid

{- DList is Difference List
 -
 - Suppose we are appending lists
 - a ++ b
 - The definition of (++) make this operation's cost depends on the left operand
 - So if we append repeatedly, the left operand increases in size
 - Where append in imperative is linear, using (++) directly repeated is quadratic
 -
 - The work around is Difference List
 - e.g.
 - let f = ("a" ++) . ("b" ++ )
 - f []
 - > "ab"
 -
 - What's happening is we are using partial functions and function composition
 - We can have a very long chain, but it does not actually append yet
 - When we apply it onto [] then we unleash the append
 - Note that since we did function composition, each function is applied one by one,
 - thus only costs as much that function costs i.e. linear! -}

newtype DList a = DL {
  unDL :: [a] -> [a]
}

append :: DList a -> DList a -> DList a
append x y = DL (unDL x . unDL y)
-- This is where we do the composition

append' :: DList a -> DList a -> DList a
append' (DL x) (DL y) = DL (x . y)

fromList :: [a] -> DList a
fromList xs = DL (xs ++)

toList :: DList a -> [a]
toList (DL xs) = xs []
-- See? We applied xs onto [] to unleash the chaining

empty :: DList a
empty = DL id

cons :: a -> DList a -> DList a
cons x (DL xs) = DL ((x:) . xs)
infixr `cons`

dfoldr :: (a -> b -> b) -> b -> DList a -> b
dfoldr f z xs = foldr f z (toList xs)

safeHead :: DList a -> Maybe a
safeHead xs = case toList xs of
                  (y:_) -> Just y
                  _ -> Nothing

dmap :: (a -> b) -> DList a -> DList b
dmap f = dfoldr go empty
    where go x xs = cons (f x) xs

instance Functor DList where
    fmap = dmap

instance Monoid (DList a) where
    mempty = empty
    mappend = append
