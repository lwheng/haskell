{-# LANGUAGE FlexibleInstances #-}
class Borked a where
  bork :: a -> String

instance Borked Int where
  bork = show

instance Borked (Int, Int) where
  bork (a, b) = (bork a) ++ ", " ++ (bork b)

-- This syntax means we want a and b to be first an instance of Borked
-- before we make it a new instance
instance (Borked a, Borked b) => Borked (a, b) where
  bork (a, b) = ">>" ++ bork a ++ " " ++ bork b ++ "<<"

-- We have 2 instances of Borked for pairs:
-- 1. one for a pair of Ints
-- 2. one for a pair of anything that's Borked
-- Haskell compiler cannot decide which to use
-- Thus error when we use "bork"
