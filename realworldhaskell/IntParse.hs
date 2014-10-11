import Data.Char (digitToInt)

asInt :: String -> Int
asInt xs = loop 0 xs

loop acc [] = acc
loop acc (x:xs) = loop acc' xs
  where acc' = acc * 10 + digitToInt x
