interperse :: a -> [[a]] -> [a]

interperse _ [] = []
interperse _ [x] = x
interperse sep (x:xs) = x ++ [sep] ++ interperse sep xs

-- intuitively, just to join list of lists with a separator
-- The first case is for empty list
-- the second case, also terminating case, is for the list that has only one element, this also makes sure the separator doesn't appear at the end
-- the third case is the recursive case
--
-- notice the type signature uses 'a' for its 2 arguments, and also the output
-- this means they are of the same type
-- so it will work for any inputs as long they are of the same type
