import Data.Char (digitToInt)

asInt :: String -> Int

loop :: Int -> String -> Int
loop acc [] = acc
loop acc (x:xs) = loop (acc * 10 + digitToInt x) xs
-- To demonstrate using single quotes in Haskell variable names
-- loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
--                    in loop acc' xs
-- it is like how we add prime to variables or functions in math
-- x vs x'
-- f(x) vs f'(x)

asInt xs = loop 0 xs
