data List a = Cons a (List a)
            | Nil
            deriving (Show)

-- Convert a list to our custom list
-- A list [1,2,3,4] is actually
-- 1:(2:(3:(4:[])))
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

toList (Cons a b) = a : toList b
toList Nil = []
