-- There are some functions that are not entirely safe
-- e.g.
-- head []
-- Throws exception

-- So we add some checking
myDumbExample xs = if length xs > 0
                   then head xs
                   else 'Z'

mySmartExample xs = if not (null xs)
                    then head xs
                    else 'Z'

-- What's the diff? Performance. null runs at constant, length need to run through the entire list
--

-- Otherwise just use Pattern Matching
myOtherExample (x:_) = x
myOtherExample [] = 'Z'
