data CustomColor = CustomColor {
  red :: Int,
  green :: Int,
  blue :: Int
} deriving (Eq, Show, Read)

data FuncRec = FuncRec {
  name :: String,
  colorCalc :: Int -> (CustomColor, Int)
}

plus5func color x = (color, x + 5)

purple = CustomColor 255 0 255

plus5 = FuncRec {name = "plus5", colorCalc = plus5func purple}
always0 = FuncRec {name = "always0", colorCalc = \_ -> (purple, 0)}

data FuncRec2 = FuncRec2 {
  myName :: String,
  calc :: Int -> Int,
  namedCalc :: Int -> (String, Int)
}

mkFuncRec2 :: String -> (Int -> Int) -> FuncRec2
mkFuncRec2 name calcFunc = 
  FuncRec2 { myName = name,
             calc = calcFunc,
             namedCalc = \x -> (name, calcFunc x)
  }

plus5' = mkFuncRec2 "plus5" (+5)
always0' = mkFuncRec2 "always0" (\_ -> 0)
