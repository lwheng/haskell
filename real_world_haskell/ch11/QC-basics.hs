import Test.QuickCheck
import Test.QuickCheck.Gen
import Data.List
import System.Random

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs = filter (< x) xs
          rhs = filter (>= x) xs

prop_idempotent xs = qsort (qsort xs) == qsort xs

-- to test:
-- quickCheck (prop_idempotent :: [Integer] -> Bool)
-- You can use "verboseCheck" too


-- In the book is:
-- generate 10 (System.Random.mkStdGen 2) arbitrary :: [Bool]
-- THIS DON'T WORK ANYMORE BECAUSE THERE IS NO generate
-- Instead, use this:
-- unGen arbitrary (mkStdGen 42) 10 :: [Bool]


-- quickCheck (prop_idempotent :: [Integer] -> Bool)
-- verboseCheck (prop_idempotent :: [Integer] -> Bool)

-- We might be tempted to do this to check a property of a sorted list
-- However remember 'head' and 'minumum' are unsafe, that they don't work
-- on empty lists
prop_minimum xs = head (qsort xs) == minimum xs

-- So we have this
prop_minimum' xs = not (null xs) ==> head (qsort xs) == minimum xs
-- quickCheck (prop_minimum' :: [Integer] -> Property)
-- How to read it: If not (null xs), then.....
-- (==>) is implication function, filters out invalid data before running the property

-- To test a sorted list is in increasing order
prop_ordered xs =  ordered (qsort xs)
    where ordered [] = True
          ordered [x] = True
          ordered (x:y:xs) = x <= y && ordered (y:xs)

-- To test a sorted list is a permutation of the original list
-- a \\ b : This is List a minus List b
prop_permutation xs = permutation xs (qsort xs)
    where permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

-- To test that the last element is the max
prop_maximum xs = not (null xs) ==> last (qsort xs) == maximum xs

-- To test that the min of the sorted is one of the 2 list
prop_append xs ys = not (null xs) ==>
                        not (null ys) ==>
                            head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)

-- To test against a model
prop_sort_model xs = sort xs == qsort xs
