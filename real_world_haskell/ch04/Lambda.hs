safeHead (x:_) = Just x
safeHead _ = Nothing

unsafeHead = \(x:_) -> x
-- when using lambda functions, we cannot add multiple clauses
-- hence the pattern MUST match, otherwise it will explode
