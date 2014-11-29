module State where

import Control.Monad
import Control.Applicative

newtype State s a = State {
  runState :: s -> (a, s)
}

-- Remember, the word "return" is not the return used in imperative languages
-- It refers to the monad wrapper
returnState :: a -> State s a
returnState a = State $ \s -> (a, s)

bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s -> let (a, s') = runState m s
                              in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

instance Monad (State s) where
  return = returnState
  (>>=) = bindState
