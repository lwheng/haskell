{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module SupplyInstance (
    Reader
  , runReader
) where

import Control.Monad
import SupplyClass
import RandomSupply

newtype Reader e a = R {
                         runReader :: e -> a
                       }

instance Functor (Reader e) where
  fmap f r = R $ \e -> f (runReader r e)

instance Applicative (Reader e) where
  pure a  = R $ \_ -> a
  x <*> y = R $ \e -> (runReader x e) (runReader y e)

instance Monad (Reader e) where
  return a = R $ \_ -> a
  m >>= k  = R $ \r -> runReader (k (runReader m r)) r

ask
  :: Reader e e
ask = R id

newtype MySupply e a = MySupply {
                                  runMySupply :: Reader e a
                                } deriving (Functor, Applicative, Monad)

instance MonadSupply e (MySupply e) where
  next = MySupply $ do
           v <- ask
           return (Just v)

xy :: (Num s, MonadSupply s m) => m s
xy = do
       Just x <- next
       Just y <- next
       return (x * y)

runMS
  :: MySupply i a
  -> i
  -> a
runMS = runReader . runMySupply
