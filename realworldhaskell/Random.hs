module Random where

import System.Random
import State
import Control.Monad

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)

twoGoodRandoms :: RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms gen = let (a, gen') = random gen
                         (b, gen'') = random gen'
                     in ((a, b), gen'')

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom =
  get >>= \gen ->
  let (val, gen') = random gen in
  put gen' >>
  return val

getRandom' :: Random a => RandomState a
getRandom' = do
  -- Remember "get" returns a "State s s".
  -- Remember the monad constructor is "State s"
  -- When we execute the (<-) operator, we unwrap the monad and access its value
  -- In this case, "get" sets the state as its value
  -- And that's why "gen" because the StdGen
  gen <- get

  -- We run the "random" function on a StdGen to get a (Int, StdGen)
  let (val, gen') = random gen

  -- Not exactly sure why we even need this here
  -- Perhaps: We are using getStdGen and setStdGen i.e. we are using the global number generator and that's why a put here don't matter
  put gen'

  -- Remember the implementation of "return" for our State monad
  -- "return" calls "returnState"
  -- "returnState" takes a value and wraps it into a State monad
  -- In this case, a RandomState
  return val

getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom

runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
  -- Init a StdGen
  oldState <- getStdGen

  -- Use the StdGen in getTwoRandoms
  -- It returns us the randomized (Int, Int) tuple, together with the modified StdGen
  let (res, newState) = runState getTwoRandoms oldState

  -- This is the line that actually make sures that global number generator is updated
  -- Otherwise if you run runTwoRandoms again, it returns the same randomized values
  setStdGen newState
  return res
