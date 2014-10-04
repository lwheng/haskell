import Data.List
-- Definition of foldl
--foldl :: (a -> b -> a) -> a -> [b] -> a
--foldl step zero (x:xs) = foldl step (step zero x) xs
--foldl _ zero [] = zero
---- foldl take a step function, an accumulator and a list
---- step takes acc and an element from the list

foldlSum xs = foldl step 0 xs
  where step acc x = acc + x

foldrSum xs = foldr step 0 xs
  where step x acc = acc - x

niceSum :: [Integer] -> Integer
niceSum xs = foldl (+) 0 xs

-- What's happening in foldl?
-- foldl (+) 0 [1,2,3]
-- == foldl (+) (0 + 1) (2:3:[])
-- == foldl (+) ((0 + 1) + 2) (3:[])
-- == foldl (+) (((0 + 1) + 2) + 3) []
-- == (((0 + 1) + 2) + 3)
--

niceSum1 :: [Integer] -> Integer
niceSum1 xs = foldr (+) 0 xs

-- What's happening in foldr?
-- foldr (+) 0 [1,2,3]
-- == 1 + foldr (+) 0 (2:3:[])
-- == 1 + (2 + foldr (+) 0 (3:[])
-- == 1 + (2 + (3 + foldr (+) 0 ([])
-- == 1 + (2 + (3 + 0))
--
-- To quickly see the difference between foldl and foldr
-- foldr (\x y -> x - y) 0 [1,2,3] == 2
-- foldl (\x y -> x - y) 0 [1,2,3] == -6

-- Avoid space leaks with seq
--
-- let new = 1 + 2
-- in new `seq` foldl' (+) new []
-- seq forces its first argument to be evaluated
-- hence we don't get a thunk for that expression
--
-- Learning how to use seq
--
hiddenInside x y = someFunc (x `seq` y)
-- Too late, seq lies inside the call of someFunc
onTheOutside x y = x `seq` someFunc y
-- correct, x is evaluated first before anything
chained x y z = x `seq` y `seq` someFunc z
-- corect, x and y are evaluated first
--
-- One limitation of seq is it will stop evaluating when it sees an constructor
-- so, applying seq to (1+2):(3+4):[] will only evaluate (1+2) because it stops on seeing (:)
-- to overcome this:
strictPair (a,b) = a `seq` b `seq` (a,b)

strictList (x:xs) = x `seq` x : strictList xs
strictList [] = []

someFunc = undefined
