import Data.List
import Data.Function
sortByLength :: [[a]] -> [[a]]
sortByLength = sortBy (compare `on` length)

-- sortBy
-- from Data.List
-- sortBy <Some way to order the list> <List>
--
-- `on`
-- from Data.Function
-- f `on` g = \x y -> f (g x) (g y)
-- i.e. apply g first, then apply f
--
-- in this case, compare `on` length
-- i.e. compare (length x) (length y)
-- `on` was used infix
-- can also use like this: on compare length
-- but compare `on` length seems more English
--
-- compare takes 2 arguments of the same type and compares them
-- compare returns LT, GT, EQ etc.
--
-- intuitively, take 2 items from list, apply length, apply compare, then sortBy that, and then continue
