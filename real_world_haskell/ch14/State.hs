-- In SimpleState.hs, we use a type synonym instead of a type definition for SimpleState.
-- 
-- In order to define a Monad instance, we have to provide a proper type constructor as well as definitions for (>>=) and return

newtype State s a = State {
      runState :: s -> (a, s)
    }

returnState :: a -> State s a
returnState a = State $ \s -> (a, s)

bindState :: State s a -> (a -> State s b) -> State s b
bindState m k = State $ \s -> let (a, s') = runState m s
                              in runState (k a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)
