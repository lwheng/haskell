-- Multiple constructors to the type

data Roygbiv = Red
              | Orange
              | Yellow
              | Green
              | Blue
              | Indigo
              | Violet
              deriving (Eq, Show)

r = Red
o = Orange
r == o
-- Not equal
