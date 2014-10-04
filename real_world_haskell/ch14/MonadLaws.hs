-- First law: return is a left identity for (>>=)
return x >>= f === f x
-- OR
do y <- return x --- This is redundant. We are wrapping for the sake of unwrapping
   f y         === f x


-- Second law: return is a right identity for (>>=)
m >>= return   === m
-- OR
do y <- m
   return y    === m
-- Similarly, we are unwrapping and wrapping again
--

-- Final law: Associativity
m >>= (\x -> f x >>= g) === (m >>= f) >>= g
