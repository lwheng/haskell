module BusTicket (
    main
) where

main :: IO ()
main = print $ isConsective [1,2,3,4]

isConsective :: [Int] -> Bool
isConsective [] = True
isConsective [x] = True
isConsective (x:y:xs) = x+1 == y && isConsective (y:xs)
