-- Intro to "first" and "second"

-- first (+3) (1,2) === (4,2)
-- second odd ('a', 1) === ('a', True)


import Control.Arrow (first)
import System.Random

randomsIO_golfed :: Random a => IO [a]
randomsIO_golfed = getStdRandom (first randoms . split)
