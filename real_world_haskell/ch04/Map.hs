import Data.Char (toUpper)
square :: [Double] -> [Double]

square [] = []
square (x:xs) = x*x : square xs

upperCase :: String -> String

upperCase [] = []
upperCase (x:xs) = toUpper x : upperCase xs

-- What we just did, is to apply a function to the head of a list, and traverse the list and apply it to the rest of the list using recursion
-- The function, map, allow us to write them more elegantly


square1 :: [Double] -> [Double]
square1 xs = map (\x -> x*x) xs
-- OR
square2 :: [Double] -> [Double]
square2 xs = map squareIt xs
  where squareIt x = x * x

upperCase2 = map toUpper

-- To understand what is map:
myMap :: (a -> b) -> [a] -> [b]
myMap f (x:xs) = f x : myMap f xs
myMap _ _ = []
