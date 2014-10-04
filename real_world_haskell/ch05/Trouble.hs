-- Type inference is double-edged sword

import Data.Char
upcaseFirst (c:cs) = toUpper c -- forgot ":cs" here
-- the compiler will infer that it returns Char

camelCase :: String -> String
camelCase xs = concat (map upcaseFirst (words xs))
-- compilation fails
--

-- As this stage, use type signature since we are not so experienced and confident
-- with our functions
-- Use type signatures to help us catch bugs
