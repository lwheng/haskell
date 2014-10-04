{-# LANGUAGE DatatypeContexts #-}
-- Restriction for fmap
-- Can only make instances of Functor from types that have exactly ONE type parameter
-- e.g. Maybe a, Tree a
-- Cannot make instances of these: Either a b, Bool, Int (Because they don't have exactly ONE type parameter)
--
-- Also, we can't place any constraints on the type definitions

data Foo a = Foo a

instance Functor Foo where
    fmap f (Foo a) = Foo (f a)
-- this compiles

-- We cannot do this anymore
-- because DatatypeContexts is deprecated
data Eq a => Bar a = Bar a

instance Functor Bar where
    fmap f (Bar a) = Bar (f a)
-- cannot compile!
