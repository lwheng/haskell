returnSingleton :: a -> [a]
returnSingleton x = [x]

-- Let's examine type of (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- So now we work with the List monad. Intuitively it should be:
-- List a -> (a -> List b) -> List b
-- Which is the same as:
-- [a] -> (a -> [b]) -> [b] ----- (1)
--
-- Now we look at definition for "map"
-- map :: (a -> b) -> [a] -> [b] ----- (2)
--
-- We can see that (1) and (3) are a little similar
-- flip map :: [a] -> (a -> b) -> [b] ----- (3)
-- If we sub [b] for b in (3):
-- flip map :: [a] -> (a -> [b]) -> [[b]] ----- (4)
-- Then we are just one term different between (1) and (4)
--
-- concat :: [[a]] -> [a]
-- With that, we can get:
--
-- instance Monad [] where
--     return x = [x]
--     xs >>= f = concat (map f xs)
