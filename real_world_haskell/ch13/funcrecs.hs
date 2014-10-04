{- | Our usual CustomColor type to play with -}
data CustomColor = 
    CustomColor { red :: Int,
                  green :: Int,
                  blue :: Int}
    deriving (Eq, Show, Read)

{- | A new type that stores a name and a function
 -
 - The function takes an Int, applies some computation to it, and returns
 - an Int along with a CustomColor -}

data FuncRec = 
    FuncRec { name :: String,
              colorCalc :: Int -> (CustomColor, Int)}

plus5func color x = (color, x+5)

purple = CustomColor 255 0 255

-- Intuitively...
-- "plus5func purple" is waiting for an Int to output (color, x+5)
-- When we set it as the colorCalc of plus5, colorCalc is waiting for an Int
-- and returns a (CustomColor, Int) tuple --> colorCalc :: Int -> (CustomColor, Int)
-- We also manage to pass "purple" into FuncRec even though it does have a field to store a CustomColor
-- This, is closure in action
plus5 = FuncRec {name = "plus5", colorCalc = plus5func purple}
always0 = FuncRec {name = "always0", colorCalc = \_ -> (purple, 0)}
