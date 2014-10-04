data Color = Red
            | Green
            | Blue

colorEq :: Color -> Color -> Bool
colorEq Red Red = True
colorEq Green Green = True
colorEq Blue Blue = True
colorEq _ _ = False

stringEq :: String -> String -> Bool
stringEq [] [] = True
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys
-- We cheat by using the (==) function

stringEq _ _ = False

-- Notice that at this point, for each type, we have to define a <type>Eq function
-- This is annoying
-- We should be able to just use (==) to compare anything of the same type
-- Hence typeclasses
