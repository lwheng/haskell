import Control.Monad (liftM2)
import Control.Monad.State

import System.Random

rand :: IO Int
rand = getStdRandom (randomR (0, maxBound))

twoBadRandoms :: RandomGen g => g -> (Int, Int)
twoBadRandoms gen = (fst $ random gen, fst $ random gen)
-- Call this by:
-- twoBadRandoms `fmap` getStdGen
-- Recall that we are in pure code, so the RandomGen is immutable
-- So when we use the same generator within the funciton, we always get back the same "random" number

-- If we examine the type of "random"
-- random :: (RandomGen g, Random a) => g -> (a, g)
-- We notice that if takes in a generator and returns a tuple
-- Also observe that twoGoodRandoms is very similar to the State Monad
twoGoodRandoms :: RandomGen g => g -> ((Int, Int), g)
twoGoodRandoms gen = let (a, gen') = random gen
                         (b, gen'') = random gen'
                     in ((a, b), gen'')

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom = 
    get >>= \gen ->
    let (val, gen') = random gen
    in put gen' >>
    return val
-- This reads: get the current generator, use it get a random "val", put the new generator into the state

getTwoRandoms :: Random a => RandomState (a, a)
getTwoRandoms = liftM2 (,) getRandom getRandom

runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
  -- getStdGen :: IO StdGen
  oldState <- getStdGen
  let (result, newState) = runState getTwoRandoms oldState
  setStdGen newState
  return result

-- Suppose now you want a complex state value to pass around
-- Simple example, the generator and the number of times it was used
data CountedRandom = CountedRandom {
        crGen :: StdGen
      , crCount :: Int
      }
type CRState = State CountedRandom
getCountedRandom :: Random a => CRState a
getCountedRandom = do
  -- First get the state
  st <- get
  -- crGen will get the generator from the state, then we use it
  let (val, gen') = random (crGen st)
  -- Once we used it, we put the new generator back into the state, and increase the crCount
  put CountedRandom { crGen = gen', crCount = crCount st + 1 }
  -- Then we wrap the results
  return val

getCount :: CRState Int
getCount = crCount `liftM` get

putCount :: Int -> CRState ()
putCount a = do
  st <- get
  put st { crCount = a }

putCountModify :: Int -> CRState ()
putCountModify a = modify $ \st -> st { crCount = a }
