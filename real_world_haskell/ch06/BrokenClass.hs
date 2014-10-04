{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module BrokenClass
where

import JSONClass

-- This syntax means we want this instance to be first an instance
-- of JSON
instance (JSON a) => JSON [a] where
  toJValue = undefined
  fromJValue = undefined

instance (JSON a) => JSON [(String, a)] where
  toJValue = undefined
  fromJValue = undefined

-- In this file we demonstrate "overlapping isntances",
-- a problem arising from Haskell "open world assumption"
-- We can compile, but when we run it, Haskell cannot decide
-- which one to use
