module Supply (
    main

  , Supply
  , next
  , runSupply
) where

import Control.Monad.State

main :: IO ()
main = return ()

newtype Supply s a = S (State [s] a)

next
  :: Supply s (Maybe s)
next = undefined

runSupply
  :: Supply s a
  -> [s]
  -> (a, [s])
runSupply = undefined

