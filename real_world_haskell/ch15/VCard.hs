import Control.Monad

data Context = Home | Mobile | Business
               deriving (Eq, Show)

type Phone = String

albulena = [(Home, "+355-652-55512")]

nils = [(Mobile, "+47-922-55-512"), (Business, "+47-922-12-121"),
        (Home, "+47-925-55-121"), (Business, "+47-922-25-551")]

twalumba = [(Business, "+260-02-55-5121")]

-- Suppose we want to call someone. We don't want to call their business number
-- and we prefer to call their home number over mobile number
onePersonalPhone :: [(Context, Phone)] -> Maybe Phone
onePersonalPhone ps = case lookup Home ps of
                        Nothing -> lookup Mobile ps
                        Just n -> Just n

allBusinessPhones ps = map snd numbers
    where numbers = case filter (contextIs Business) ps of
                      [] -> filter (contextIs Mobile) ps
                      ns -> ns
contextIs a (b, _) = a == b

-- Introducing MonadPlus
-- class Monad m => MonadPlus m where
--    mzero :: m a  
--    mplus :: m a -> m a -> m a
--
--    instance MonadPlus [] where
--       mzero = []
--       mplus = (++)
--
--    instance MonadPlus Maybe where
--       mzero = Nothing

--       Nothing `mplus` ys  = ys
--       xs      `mplus` _ = xs
--
-- mzero represents an empty result, mplus combines 2 results into one
-- Note that mplus does NOT necessary means addition. It has to be depend on the Monad that is was defined with

oneBusinessPhone :: [(Context, Phone)] -> Maybe Phone
oneBusinessPhone ps = lookup Business ps `mplus` lookup Mobile ps

allPersonalPhones :: [(Context, Phone)] -> [Phone]
allPersonalPhones ps = map snd $ filter (contextIs Home) ps `mplus` filter (contextIs Mobile) ps
