{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Supply
    (
      Supply
    , next
    , runSupply
    ) where

import Control.Monad.State

newtype Supply s a = S (State [s] a)
                     deriving (Monad)
-- The "s" parameter is the type of the unique values we are going to supplu
-- The "a" is the type parameter in order to make our type a monad

-- Given a Supply action and list of values, run the action on the list of values
-- Because Supply is like an extension of the State monad, runSupply makes use of State monad's runState to run the state
runSupply :: Supply s a -> [s] -> (a, [s])
runSupply (S m) xs = runState m xs

-- next is an action
-- next uses "get" to get the state out of the State monad
-- (<-) takes the value out of the state, i.e. [s]
-- Then it does pattern matching
-- If [], then wraps Nothing
-- otherwise, wraps the head of the list, then puts the rest back into the state
-- finally, wrap it with S
next :: Supply s (Maybe s)
next = S $ do st <- get
              case st of
                [] -> return Nothing
                (x:xs) -> do put xs
                             return (Just x)
