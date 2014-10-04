-- This file won't compile, just some examples

all odd [1,3,5]
-- all takes a boolean function and a list
-- so if all the elements satisfy that function, then True
--
-- quite similarly
any odd [1,2,3,4,5]


-- splitAt
-- A combi of take and drop
-- returns a tuple of  (take n, drop n)
splitAt 3 "foobar"
