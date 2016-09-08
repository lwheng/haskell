newtype Reader e a = R {
                         runReader :: e -> a
                       }

instance Functor (Reader e) where
  fmap = undefined
-- instance Monad (Reader e) where
--   return a = R $ \_ -> a
--   m >>= k  = R $ \r -> runReader (k (runReader m r)) r
