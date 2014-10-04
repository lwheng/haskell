{-# LANGUAGE DatatypeContexts #-}
-- Constrains on Type Definitions are bad
-- Consider:

data (Ord a) => OrdStack a = Bottom
                           | Item a (OrdStack a)
                           deriving (Show)
-- A simple definition for a stack data type

isIncreasing :: (Ord a) => OrdStack a -> Bool
isIncreasing (Item a rest@(Item b _))
    | a < b = isIncreasing rest
    | otherwise = False
isIncreasing _ = True
-- Recall the syntax: rest@(Item b _). Treat this as an alias, rest is an alias for (Item b _)
-- This function, isIncreasing, needs Ord to allow us to perform pairwise comparisons

-- Now consider a push function that adds items to our stack
-- This won't compile! Because we didn't add (Ord a)
{-
push :: a -> OrdStack a -> OrdStack a
push a s = Item a s
-}

-- Because we added the constraint to the type, we have to add it every single function
-- In fact because of this, DatatypeContexts is deprecated
-- We can no longer define a type with contraints
push :: (Ord a) => a -> OrdStack a -> OrdStack a
push a s = Item a s
