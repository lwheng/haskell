-- Type signature
mySecond :: [a] -> a

-- this version throws an error when the list is too short
mySecond xs = if null (tail xs)
              then error "List is too short"
              else head (tail xs)

safeSecond :: [a] -> Maybe a

-- this version is better. because throwing errors will force a crash
safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))
