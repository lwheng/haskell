import Control.Monad
import Control.Monad.Trans

newtype MaybeT m a = MaybeT {
                              runMaybeT :: m (Maybe a)
                            }

bindMT
  :: (Monad m)
  => MaybeT m a
  -> (a -> MaybeT m b)
  -> MaybeT m b
x `bindMT` f = MaybeT $ do -- Note that this "do block" is exectued in the underlying monad m and not (MaybeT m)
                 unwrapped <- runMaybeT x
                 case unwrapped of
                   Nothing -> return Nothing
                   Just y  -> runMaybeT (f y)

-- This makes it clearer to see that we executing in the underlying monad
altBindMT
  :: (Monad m)
  => MaybeT m a
  -> (a -> MaybeT m b)
  -> MaybeT m b
x `altBindMT` f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

returnMT
  :: (Monad m)
  => a
  -> MaybeT m a
returnMT a = MaybeT $ return (Just a) -- Note that the "return" is for underlying monad m

failMT
  :: (Monad m)
  => t
  -> MaybeT m a
failMT _  = MaybeT $ return Nothing -- Why "return Nothing" too?

mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

instance (Functor m) => Functor (MaybeT m) where
  fmap f = mapMaybeT (fmap (fmap f))

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

instance (Functor m, Monad m) => Applicative (MaybeT m) where
  pure      = lift . return
  mf <*> mx = MaybeT $ do
                maybeF <- runMaybeT mf
                case maybeF of
                  Nothing -> return Nothing
                  Just f  -> do
                    maybeX <- runMaybeT mx
                    case maybeX of
                      Nothing -> return Nothing
                      Just x  -> return (Just (f x))

instance (Monad m) => Monad (MaybeT m) where
  return = returnMT
  (>>=)  = bindMT
  fail   = failMT

instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO m = lift (liftIO m)

instance (MonadState s m) => MonadState s (MaybeT m) where
  get = lift get
  put k = lift (put k)
