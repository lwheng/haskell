-- Bad example because never handle empty list
badExample (x:xs) = x + badExample xs

goodExample (x:xs) = x + goodExample xs
goodExample _ = []
