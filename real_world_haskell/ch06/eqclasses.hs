data Color = Red | Green | Blue

class BasicEq a where
  isEqual :: a -> a -> Bool
-- We define a typeclass by using the "class" keyword
-- BasicEq is the name
-- "a" refers to instance types
-- i.e. "For all types 'a', so long as 'a' is an instance of BasicEq,
-- isEqual takes 2 parameters of the type 'a' and returns a Bool

instance BasicEq Bool where
  isEqual True True = True
  isEqual False False = False
  isEqual _ _ = False
-- Here we defined Bool as an instance of BasicEq



class BasicEq2 a where
  isEqual2 :: a -> a -> Bool
  isNotEqual2 :: a -> a -> Bool
-- If we define the typeclass like this, the instance will have to define both functions


class BasicEq3 a where
  isEqual3 :: a -> a -> Bool
  isEqual3 x y = not (isNotEqual3 x y)

  isNotEqual3 :: a -> a -> Bool
  isNotEqual3 x y = not (isEqual3 x y)
-- If we define the typeclass like this, the instance must define ONE of them
-- Let's try it
instance BasicEq3 Color where
  isEqual3 Red Red = True
  isEqual3 Green Green = True
  isEqual3 Blue Blue = True
  isEqual3 _ _ = False
instance Show Color where
  show Red = "Red"
  show Blue = "Blue"
  show Green = "Green"

instance Read Color where
  -- readsPrec is the main function for parsing input
  -- In here, we are defining the readsPrec function for Color
  readsPrec _ value =
    -- We pass tryParse a list of pairs. Each pair is a String and desired return value
    -- tryParse will try to match the input to one of these strings
    tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
    where tryParse [] = []
          tryParse ((attempt, result):xs) = 
            -- Compare the start of the string to be parse to the text we are looking for
            if (take (length attempt) value) == attempt
              -- if we have a match, return the result and the remaining input
              then [(result, drop (length attempt) value)]
              -- if we don't, try the next pair
              else tryParse xs
