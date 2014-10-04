-- Supplying random numbers
--
-- First, we can get random numbers using StdGen. We can get StdGen in the IO monad, but we must "put back" a different/modified StdGen when we're done (otherwise we will keep getting the same random number)
--
-- getStdRandom ensures that when we get one StdGen, we put one back
-- :type getStdRandome
-- getStdRandom :: (StdGen -> (a, StdGen)) -> IO a
--
-- We can use "random" to get back a new StdGen, we can use "randoms" to get an infinite list of random numbers. But how to get both?
--
-- The answer is "split"

import Supply
import System.Random hiding (next)
-- NEW SYNTAX: hiding
-- I'm guessing, import everything EXCEPT hiding

randomsIO :: Random a => IO [a]
randomsIO =
    getStdRandom $ \g ->
        let (a,b) = split g
        in (randoms a, b)
