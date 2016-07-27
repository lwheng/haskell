module Tautology (
    main
) where

main :: IO ()
main = return ()

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop
          deriving (Show)

type Assoc k v = [(k,v)]
type Subst = Assoc Char Bool

-- A ^ ¬A
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

-- (A ^ B) -> A
p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

-- A -> (A ^ B)
p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

-- (A ^ (A -> B)) -> B
p4 :: Prop
p4 = Imply (And (Var 'A') (Imply (Var 'A') (Var 'B'))) (Var 'B')

eval
  :: Subst
  -> Prop
  -> Bool
eval _ (Const b)   = b
eval s (Var c)     = find c s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q

find
  :: Char
  -> Subst
  -> Bool
find c s = case lookup c s of
             Nothing -> False -- We default to False if it is not found
             Just b  -> b

vars
  :: Prop
  -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q

bools
  :: Int
  -> [[Bool]]
bools n = case n of
            0 -> [[]]
            n -> if n > 0
                   then map (False:) bss ++ map (True:) bss
                   else error "n must be non-negative"
  where
    bss = bools (n-1)

rmdups
  :: Eq a
  => [a]
  -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

substs
  :: Prop
  -> [Subst]
substs p = map (zip vs) (bools (length vs))
  where
    vs = rmdups (vars p)

isTaut
  :: Prop
  -> Bool
isTaut p = and [eval s p | s <- substs p]
