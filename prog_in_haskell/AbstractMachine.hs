module AbstractMachine (
    main
) where

main :: IO ()
main = return ()

data Expr = Val Int
          | Add Expr Expr
          deriving (Show)

data Op = EVAL Expr
        | ADD Int
        deriving (Show)

type Cont = [Op]

value
  :: Expr
  -> Int
-- value (Val x)   = x
-- value (Add x y) = value x + value y
value e = eval e []

-- | This eval function enforces that in an (Add x y), x is evaluated first
eval
  :: Expr
  -> Cont
  -> Int
eval (Val n) c   = exec c n
eval (Add x y) c = eval x (EVAL y : c)

exec
  :: Cont
  -> Int
  -> Int
exec []           n = n
exec (EVAL y : c) n = eval y (ADD n : c)
exec (ADD  n : c) m = exec c (n + m)
