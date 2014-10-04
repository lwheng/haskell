-- Demonstrating different constructors for the same type
-- Note that even though they are the same type, instances created with different constructors are different

type Vector = (Double, Double)
type Radius = Double

data Shape = Circle Vector Radius
            | Poly [Vector]
            deriving (Show)
