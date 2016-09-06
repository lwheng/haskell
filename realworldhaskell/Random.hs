module Random where

import Control.Monad
import State
import System.Random

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
  -- And that's why "gen" is the StdGen
  gen <- get

  -- We run the "random" function on a StdGen to get a (Int, StdGen)
  let (val, gen') = random gen

  -- Put the new StdGen into the State
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

data CountedRandom = CountedRandom {
    crGen :: StdGen
  , crCount :: Int
}

type CRState = State CountedRandom
-- type CRState a = State CountedRandom a

getCountedRandom :: Random a => CRState a
getCountedRandom = do
  st <- get
  let (val, gen') = random (crGen st)
  put CountedRandom { crGen = gen', crCount = crCount st + 1 }
  return val

getCount :: CRState Int
getCount = crCount `liftM` get

putCount :: Int -> CRState ()
putCount a = do
  st <- get
  put st { crCount = a }

runCountedRandom :: IO Int
runCountedRandom = do
  gen <- getStdGen

  let cr = CountedRandom { crGen = gen, crCount = 0} 
  let (res, newState) = runState getCountedRandom cr

  setStdGen (crGen newState)
  return res
