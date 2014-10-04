data Fruit = Apple | Orange
  deriving (Show)

apple = "apple"
orange = "orange"

-- Type Signature
whichFruit :: String -> Fruit

--whichFruit f = case f of
--                apple -> Apple
--                orange -> Orange

-- inside the CASE above, apple is not referring to the 'apple' defined above, it is instead a local pattern variable

-- this is correct way
whichFruit f = case f of
                "apple" -> Apple
                "orange" -> Orange
