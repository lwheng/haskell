import Data.Char (digitToInt, ord)
import Data.List
-- Writing safe functions
--

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast :: [a] -> Maybe a
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs
safeLast [] = Nothing

safeInit :: [a] -> Maybe [a]
safeInit [x] = Just []
safeInit (x:xs) = maybe Nothing (Just . (x:)) (safeInit xs)
safeInit [] = Nothing

-- maybe
-- From Hoogle:
-- maybe :: b -> (a -> b) -> Maybe a -> b
-- The maybe function takes a default value, a function, and a Maybe value. If the Maybe value is Nothing, the function returns the default value. Otherwise, it applies the function to the value inside the Just and returns the result.
-- i.e. maybe <Default Value> <function> <Maybe value>
-- <Default Value> = Nothing
-- <function> = (Just . (x:))
--    this function itself is also quite interesting
--    it is like f.g(x), where f is Just, g is (x:). And g is expecting a list. So once just g is applied, then apply f. 
-- <Maybe value> = (safeInit xs)
--    Just using recursion
--

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs = let (y, ys) = break p xs
                 in
                    y : splitWith p (dropWhile p ys)
-- `p` is the Bool function
-- xs is the input list
-- `break` will split the list into a tuple on the first element that returns True with `p`.
-- e.g. break odd [2,3,4,5] = ([2],[3,4,5])
--
-- In this question, we want `splitWith` to perform like `words`
-- `words` splits sentences on whitespaces, dropping them in the process
-- Hence the usage of `dropWhile`, which drops elements from the list while `p` returns True
--

-- Using fold to define asInt
loop :: Int -> String -> Int
loop acc [] = acc
loop acc xs = case head xs of
  '-' -> -1 * loop acc (tail xs)
  _ -> foldl (\acc x -> acc * 10 + digitToInt x) 0 xs

asInt_fold :: String -> Int
asInt_fold [] = error "Empty string"
asInt_fold "-" = error "Invalid number"
asInt_fold ('-':xs) = negate (asInt_fold xs) -- negate will times -1 to a number
asInt_fold xs = foldl (\acc x -> (acc*10) + (digitToInt x)) 0 xs
-- digitToInt converts a digit (in Char) to a Int

asInt_fold' :: String -> Int
asInt_fold' [] = asInt_fold []
asInt_fold' "-" = asInt_fold "-"
asInt_fold' ('-':xs) = asInt_fold ('-':xs)
asInt_fold' xs = foldl step 0 xs
    where step acc c
            | c `elem` ['0'..'9'] = let acc' = acc * 10 + ord c - ord '0' -- ord returns the ASCII value of a character.
                                                                          -- so the expression "ord c - ord '0'" returns a result similarly to digitToInt
                                    in
                                      if acc' < acc
                                      then error "Number overflow" -- throwing an error
                                      else acc'
            | otherwise = error ("Non digit: " ++ show c) -- show converts anything to String
-- Damn it. So the | has be to indent at least one space right of the step definition

type ErrorMessage = String
asInt_either :: String -> Either ErrorMessage Int -- It feels better using Either than using Maybe
                                                  -- See asInt_either' for more on Either's Left and Right
asInt_either "-" = Left "Not a number"
asInt_either ('-':xs) = case asInt_either' xs of
                        Left err -> Left err
                        Right val -> Right (negate val)
asInt_either xs = asInt_either' xs

asInt_either' [] = Left "Empty String"
asInt_either' xs = foldl step (Right 0) xs
  where step (Right acc) c
          | c `elem` ['0'..'9'] = let acc' = acc * 10 + ord c - ord '0'
                                  in
                                    if acc' < acc
                                    then Left "Number overflow" -- When you Left it, the return type is ErrorMessage (which is String)
                                    else Right acc'             -- When you Right it, the return type is Int
                                                                -- i.e. Either <Left> <Right>
          | otherwise = Left ("Non-digit: " ++ show c)
        step err _ = err                                        -- This line will match against (Left _) _, which is an ErrorMessage. 

myConcat :: [[a]] -> [a]
myConcat = foldr step []
  where step x acc = x ++ acc
-- I guess sometimes you can say foldr will give you the list's elements starting from the right
-- Seems easier to think this way
-- so in myConcat we keep putting the element provided from foldr to the front of acc

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile f = helper f []
  where helper fun acc (h:t) = if fun h                           -- Check the head
                               then helper fun (acc ++ [h]) t     -- if True, we append to acc, and then call helper on the tail
                               else acc                           -- on the first element that returns False, we return what accumulated so far
        helper _ acc [] = acc                                     -- Base case

myTakeWhile' :: (a -> Bool) -> [a] -> [a]
myTakeWhile' f = foldr step []
  where step x acc | f x = x:acc
                   | otherwise = []
-- Starts from the back of list, check if True, then add in front of acc
-- On first element that return False, reset acc to []
--

-- in Data.List there's a function called groupBy
-- Exercise: figure out what groupBy
-- Answer: groupBy take the following:
-- 1. A function that returns a Bool
-- 2. A list
-- E.g. groupBy (\x y -> x > y) [3,2,2,3,2,4]
-- returns: [[3,2,2],[3,2],[4]]
-- groupBy takes 2 elements, apply the function, if True, creates a sublist and append to the final list
-- At this point, the first element, x, is kept to be compared with the 3rd element

-- Seems impossible to do using foldr
myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy f = foldr step []
  where step x [] = [[x]]
        step x ((y:ys):zs) | f x y = (x:y:ys):zs
                           | otherwise = [x] : ((y:ys):zs) 

myGroupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy' f = foldl step []
  where step [] x = [[x]]
        step acc x | f (head (last acc)) x = (init acc) ++ [last acc ++ [x]]
                   | otherwise = (init acc) ++ [last acc] ++ [[x]]

-- A bit weird to use fold to rewrite any since we don't need to traverse the entire list
fold_any :: (a -> Bool) -> [a] -> Bool
fold_any _ [] = False
fold_any f xs = head (foldr step [] xs)
  where step _ [] = [False] -- init to false
        step _ [True] = [True] -- if found True, then always True
        step x acc | f x = [True] --  found one, True
                   | otherwise = [False] -- not the droid you're looking for

fold_words :: String -> [String]
fold_words [] = []
