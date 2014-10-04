import qualified Data.Map as M

type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = StarHub
                   | SingTel
                   | MobileOne
                     deriving (Eq, Ord)

findCarrierBillingAddress :: PersonName
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress
findCarrierBillingAddress = undefined

-- This is the dreaded ladder of code marching off the right of the screen
-- See that we are repeating on using "case"
validation1 person phoneMap carrierMap addressMap = 
    case M.lookup person phoneMap of
      Nothing -> Nothing
      Just number ->
          case M.lookup number carrierMap of
            Nothing -> Nothing
            Just carrier -> M.lookup carrier addressMap

-- First improvement
validation2 person phoneMap carrierMap addressMap = do
    number <- M.lookup person phoneMap
    carrier <- M.lookup number carrierMap
    address <- M.lookup carrier addressMap
    return address

-- The "return" is not necessary. Recall that (<-) is like taking the value out of the Monad, (return) is wrapping it
-- So why unwrap and wrap again?
validation2a person phoneMap carrierMap addressMap = do
    number <- M.lookup person phoneMap
    carrier <- M.lookup number carrierMap
    M.lookup carrier addressMap

{-
 - First let's look at the definition of M.lookup
 - lookup :: Eq a => a -> [(a,b)] -> Maybe b
 - When we flip it:
 - lookupFlipped :: Eq a => [(a,b)] -> a -> Maybe b
 -
 - So it is possible to write a one-liner function by chaining them
 - You should be able to see that the output of each function is a Maybe, the input is also a Maybe
 - -}
validation3 person phoneMap carrierMap addressMap =
    lookup phoneMap person >>= lookup carrierMap >>= lookup addressMap
  where lookup = flip M.lookup

