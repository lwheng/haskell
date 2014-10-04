data Op = Plus | Minus | Mul | Div | Pow
    deriving (Eq, Show)

data SymbolicManip a =
    Number a
    | Arith Op (SymbolicManip a) (SymbolicManip a)
    deriving (Eq, Show)

instance Num a => Num (SymbolicManip a) where
    a + b = Arith Plus a b
    a - b = Arith Minus a b
    a * b = Arith Mul a b
    negate a = Arith Mul (Number (-1)) a
    abs a = error "abs is not implemented"
    signum _ = error "signum is not implemented"
    fromInteger i = Number (fromInteger i)
