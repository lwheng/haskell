{-# LANGUAGE TypeSynonymInstances, OverlappingInstances, FlexibleInstances #-}

module SimpleClass
where

import Data.List

class Foo a where
  foo :: a -> String

instance Foo a => Foo [a] where
  foo = concat . intersperse ", " . map foo
-- intersperse adds ", " between the elements of the list
--

instance Foo Char where
  foo c = [c]

instance Foo String where
  foo = id

-- In this example even though we have an instance of Foo [a] and Foo String,
-- because we added a new extension, GHC will choose the most specific instance
-- Notice the new extension added, OverlappingInstances
-- This tells GHC to choose the most specific instance when try to run 
-- foo "hello"
--
