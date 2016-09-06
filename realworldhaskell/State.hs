module State where

import Control.Applicative
import Control.Monad

newtype State s a = State {
                            runState :: s -> (a, s)
                          }

returnState
  :: a
  -> State s a
returnState a = State $ \s -> (a, s)

bindState
  :: State s a
  -> (a -> State s b)
  -> State s b
bindState m k = State $ \s -> let
                                (a, s') = runState m s
                              in
                                runState (k a) s'

get
  :: State s s
get = State $ \s -> (s, s)

put
  :: s -> State s ()
put s = State $ \_ -> ((), s)

instance Functor (State s) where
  fmap f st = State $ \s -> let
                              (a, s') = runState st s
                            in
                              (f a, s')

instance Applicative (State s) where
  pure = returnState
  (<*>) = ap
  -- x <*> y = State $ \s -> let
  --                           (func, s') = runState x s
  --                         in
  --                           runState (fmap func y) s'

instance Monad (State s) where
  return = returnState
  (>>=) = bindState
