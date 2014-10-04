-- Write a function that returns length of list

listLength :: [a] -> Int
listLength = foldr (\x acc -> (acc + 1)) 0 
-- using this chance to try out foldr
-- 1. don't need to specify the param e.g. listLength a = ...
-- 2. Take close look at the lambda function
--  x refers to an element in the list
--  acc is the accumulator, 0
--

-- Now compare with foldl
listLength' :: [a] -> Int
listLength' = foldl (\acc x -> acc + 1) 0 


-- Write a function that returns mean of a list
listMean :: [Int] -> Double
listMean xs = fromIntegral (sum xs) / fromIntegral (listLength xs)


-- Change a list into a palindrome one
listPalindrome :: [a] -> [a]
listPalindrome xs = foldr (\x acc -> acc ++ [x]) xs xs
-- e.g.
-- foldr f [1,2,3] [1,2,3]
-- = f 1 (foldr [1,2,3] [2,3])
-- = f 1 (f 2 (foldr [1,2,3] [3]))
-- = f 1 (f 2 (f 3 [1,2,3] []))
-- = f 1 (f 2 ([1,2,3] ++ 3))
-- = f 1 (([1,2,3] ++ 3) ++ 2)
-- = (([1,2,3] ++ 3) ++ 2) ++ 1
-- done

listPalindrome' :: [a] -> [a]
listPalindrome' xs = xs ++ reverse xs


-- Check whether a list is palindrome
-- Notice the type signature includes (Eq a) =>
-- This kinda means it creates an instance of the Eq class
-- see the Eq class: :info Eq
checkPalindrome :: (Eq a) => [a] -> Bool
checkPalindrome x = reverse x == x
