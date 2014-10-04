data Maybe a = Nothing
            | Just a
            deriving (Eq, Ord, Read, Show)

data Either a b = Left a
                | Right b
                deriving (Eq, Ord, Read, Show)

-- Comparing Maybe and Either
-- Like I thought before, Either can be more useful
-- because we can return either "what went wrong" or the actual result
-- Maybe returns Nothing, literally nothing
