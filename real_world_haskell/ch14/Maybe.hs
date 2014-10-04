-- This file is not meant to be compiled
-- Only for noting the key concepts

-- So we have the data type, Maybe
-- Type constructor is Nothing or Just
-- On both sides, we have the type parameter, a
-- So "a" is generic, and can be of any type that Maybe don't need and does not know
data Maybe a = Nothing
             | Just a

-- Our chaining function
-- Takes a monad, unwrap it, applies a function onto it, then wrap it
chain :: m a -> (a -> m b) -> m b

-- Wrap it
inject :: a -> m a

-- So the properties of a monad
-- 1. A type constructor, m. e.g. Maybe
-- 2. A function of type : m a -> (a -> m b) -> m b, that chains the output of one function into the input of another
-- 3. A function of type : a -> m a, that injects a normal value of type a with the type constructor

-- So we have the Monad typeclass
-- To define m as an instance of Monad, we have to define the chain and inject functions
class Monad m where
  -- chain
  (>>=) :: m a -> (a -> m b) -> m b
  -- inject
  return :: a -> m a

-- So to define Maybe as an instance of Monad:
instance Monad Maybe where
    Just x >>= k = k x
    Nothing >>= _ = Nothing

    Just _ >> k = k
    Nothing >> _ = Nothing

    return x = Just x
    fail _ = Nothing

-- (>>=) is a chaining function that depends on the output of the previous function
-- If we want to ignore the output and just perform the second function:
(>>) :: m a -> m b -> m b
a >> f = a >>= \_ -> f
-- Notice (>>) is defined using (>>=)

-- Another non-core Monad function is "fail"
fail :: String -> m a
fail = error
-- WARNING: many Monad instances uses "error" in the implementation of "fail", and this is not desirable

-- Executing the Maybe monad
-- "executing" a monad involves evaluating it and a returning a result that's had the monad's type wrapper removed
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n _ Nothing = n
maybe _ f (Just x) = f x
-- Here, n is the value to return if the result is Nothing
