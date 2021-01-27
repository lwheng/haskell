{-
There're 3 kinds of the bus ticket.
  1: ticket 1 cost 2 and can be used for a day.
  2: ticket 2 cost 7 and can be used for a consecutive 7 days.
  3: ticket 3 cost 25 can be used for a month. Assume month here means 30 consecutive days.

Now there's an array filled with elements. Each element value is a date for a person to travel.
This array has already been sorted in ascending order, like {1,3,4,5,11,12,23,24,30}.
The final day is 30th and the first day is 1st.

So for any given array from a person to travel, how can this person cost least?
-}

module BusTicket (
    busTicket
  , compute
) where

import Control.Monad (forM_)
import Control.Monad.Extra (loopM)

compute :: IO ()
compute = do
  forM_ week  $ \i -> printer 7  i
  forM_ month $ \i -> printer 25 i
  where
    week  = reverse [1..7]  :: [Double]
    month = reverse [1..30] :: [Double]

    printer
      :: Double
      -> Double
      -> IO ()
    printer num denom = putStrLn $ show num ++ " / " ++ show denom ++ " = " ++ show (num / denom)

-- Check if it is worth buying Ticket 3
-- to cover tickets up to 7th day from `today`
-- `today` could be 3, 7th day would (3 + 7 - 1) = 9
-- 7.0 / 7.0 = 1.0
-- 7.0 / 6.0 = 1.16
-- 7.0 / 5.0 = 1.4
-- 7.0 / 4.0 = 1.75 -- At least 4 days to be better than Ticket 1
-- 7.0 / 3.0 = 2.33
-- 7.0 / 2.0 = 3.5
-- 7.0 / 1.0 = 7.0
check7
  :: [Int]
  -> Bool
check7 [] = False
check7 (x:xs) = isWorthIt
  where
    daysBetween = takeToNthDay x 7 xs

    isWorthIt = length daysBetween >= 4

-- Check if it is worth buying Ticket 3
-- to cover tickets up to 25th day from `today`
-- `today` could be 3, 25th day would (3 + 25 - 1) = 27
-- 25.0 / 30.0 = 0.8333333333333334
-- 25.0 / 29.0 = 0.8620689655172413
-- 25.0 / 28.0 = 0.8928571428571429
-- 25.0 / 27.0 = 0.9259259259259259
-- 25.0 / 26.0 = 0.9615384615384616
-- 25.0 / 25.0 = 1.0 -- At least 25 days to be better than Ticket 2
-- 25.0 / 24.0 = 1.0416666666666667
-- 25.0 / 23.0 = 1.0869565217391304
check25
  :: [Int]
  -> Bool
check25 [] = False
check25 (x:xs) = isWorthIt
  where
    daysBetween = takeToNthDay x 25 xs
    isWorthIt = length daysBetween >= 25

takeToNthDay
  :: Int
  -> Int
  -> [Int]
  -> [Int]
takeToNthDay start nth days = start : filter (<=lastDay) days
  where
    lastDay = start + nth - 1

busTicket :: [Int] -> IO ()
busTicket xs = loopM handler xs

handler
  :: [Int]
  -> IO (Either [Int] ())
handler days = do
  putStrLn $ "Processing: " ++ show days

  case days of
    [] -> putStrLn "We're done" >> return (Right ())
    (x:xs) -> do
      let
        rest
          | check25 (x:xs) = do
              let
                lastDay = x + 25 - 1
                (ticketDays, r) = break (> lastDay) (x:xs)
              putStrLn $ "Buy Ticket 3 for: " ++ show ticketDays
              return $ Left r
          | check7 (x:xs) = do
              let
                lastDay = x + 7 - 1
                (ticketDays, r) = break (> lastDay) (x:xs)
              putStrLn $ "Buy Ticket 2 for: " ++ show ticketDays
              return $ Left r
          | otherwise = do
              let
                (ticketDays, r) = splitAt 1 (x:xs)
              putStrLn $ "Buy Ticket 1 for: " ++ show ticketDays
              return $ Left r
      rest
