myDrop :: Int -> [a] -> [a]

myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n-1) (tail xs)

-- using guards to make it more readable
niceDrop n xs
  | n <= 0 = xs
niceDrop _ [] = []
niceDrop n (_:xs) = niceDrop (n-1) xs
