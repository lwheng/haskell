{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
module SupplyClass
    (
      MonadSupply(..)
      , S.Supply
      , S.runSupply
    ) where

-- See that in the module definition we are re-exporting runSupply and Supply,
-- this means that clients that uses the SupplyClass does not need to re-import Supply

import qualified Supply as S

-- Read: Given some type variable m that is a Monad, make it an instance of typeclass MonadSupply s
class (Monad m) => (MonadSupply s) m | m -> s where
    next :: m (Maybe s)

instance MonadSupply s (S.Supply s) where
    next = S.next

showTwo :: (Show a) => S.Supply s String
showTwo = do
    a <- next
    b <- next
    return (show "a: " ++ show a ++ ", b: " ++ show b)

showTwo_class :: (Show s, Monad m, MonadSupply s m) => m String
showTwo_class = do
    a <- next
    b <- next
    return (show "a: " ++ show a ++ ", b: " ++ show b)
