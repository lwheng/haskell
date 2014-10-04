{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
-- Previously we say we cannot write instances of Functor for types without exactly ONE type parameters
-- E.g. Maybe a, Tree a

-- Can we do it for Either Int b ?
-- b is the only type parameter

instance Functor (Either Int) where
    fmap _ (Left n) = Left n
    fmap f (Right r) = Right (f r)
-- This doesn't compile! 
-- However if we add a compile directive (as the one you see at Line 1),
-- then it works
--
-- In fact, because there already is a Functor instance for (Either a)
-- we need the second directive
--
-- My conclusion, if we need to do so much stunt just to accomplish this, don't do it
