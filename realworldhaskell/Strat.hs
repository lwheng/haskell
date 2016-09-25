module Strat where

import Control.Parallel (par, pseq)

type Done = ()

type Strategy a = a -> Done

r0 :: Strategy a
r0 = \_ -> ()

rwhnf :: Strategy a
rwhnf x = x `seq` ()

class NFData a where
  rnf :: Strategy a
  rnf = rwhnf

parList :: Strategy a -> Strategy [a]
parList strat []     = ()
parList strat (x:xs) = strat x `par` (parList strat xs)

parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strat f xs = map f xs `using` parList strat

using :: a -> Strategy a -> a
using x s = s x `seq` x
