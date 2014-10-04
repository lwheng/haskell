type SimpleState s a = s -> (a, s)
-- Recall that monad has a type constructor with a single type variable
-- We have 2 for SimpleState. ???

-- Recall all functions in Haskell are actually single parameter
-- e.g. f x y === (f x) y
-- This is also applicable to types
-- So..

type StringState a = SimpleState String a
-- There you have it. A monad with a single type variable
-- i.e. The type constructor for our monad is "SimpleState s", not "SimpleState" alone

-- So the next thing we have to do for defining a monad is to define its injection function, return
returnSt :: a -> SimpleState s a
returnSt a = \s -> (a, s)
-- So this returns a function that "tuples them up"
-- Look at how the type SimpleState was defined, they are similar

-- Then we define the chaining function, (>>=)
-- Remember: To chain, is to unwrap, apply function, wrap
bindSt :: (SimpleState s a) -> (a -> SimpleState s b) -> SimpleState s b
bindSt m k = \s -> let (a, s') = m s
                   in (k a) s'
-- For clarity
-- m == step
-- k == makeStep
-- s == oldState
bindAlt :: (s -> (a, s)) -> (a -> s -> (b, s)) -> (s -> (b, s))
bindAlt step makeStep oldState =
    let (result, newState) = step oldState
    -- step is a function of type: s -> (a, s)
    -- makeStep is a function of type: (a -> s -> (b, s))
    in  (makeStep result) newState

-- Why SimpleState s s instead of SimpleState s a??
-- Probably because we don't even need to care about the "a" since we only want to get the state
getSt :: SimpleState s s
getSt = \s -> (s, s)

putSt :: s -> SimpleState s ()
putSt s = \_ -> ((), s)
