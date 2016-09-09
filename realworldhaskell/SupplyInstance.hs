module SupplyInstance (
    Reader
  , runReader
) where

import Control.Monad

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
