{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Concurrent.STM
import Control.Monad

main :: IO ()
main = return ()

data Item = Scroll
          | Wand
          | Banjo
  deriving (Eq, Ord, Show)

newtype Gold = Gold Int
  deriving (Eq, Ord, Show, Num)

newtype HitPoint = HitPoint Int
  deriving (Eq, Ord, Show, Num)

type Balance   = TVar Gold
type Health    = TVar HitPoint
type Inventory = TVar [Item]

data Player = Player {
                       balance   :: Balance
                     , health    :: Health
                     , inventory :: Inventory
                     }

basicTransfer
  :: Gold
  -> Balance
  -> Balance
  -> STM ()
basicTransfer qty fromBal toBal = do
  fromQty <- readTVar fromBal
  toQty   <- readTVar toBal
  writeTVar fromBal (fromQty - qty)
  writeTVar toBal   (toQty + qty)

transferTest :: STM (Gold, Gold)
transferTest = do
  alice <- newTVar (12 :: Gold)
  bob   <- newTVar 4
  basicTransfer 3 alice bob
  liftM2 (,) (readTVar alice) (readTVar bob)

removeInv :: Eq a => a -> [a] -> Maybe [a]
removeInv x xs = case takeWhile (/= x) xs of
                   (_:ys) -> Just ys
                   []     -> Nothing

maybeGiveItem
  :: Item
  -> Inventory
  -> Inventory
  -> STM Bool
maybeGiveItem item fromInv toInv = do
  fromList <- readTVar fromInv
  case removeInv item fromList of
    Nothing      -> return False
    Just newList -> do
      writeTVar fromInv newList
      destItems <- readTVar toInv
      writeTVar toInv (item : destItems)
      return True

maybeSellItem
  :: Item
  -> Gold
  -> Player
  -> Player
  -> STM Bool
maybeSellItem item price buyer seller = do
  given <- maybeGiveItem item (inventory seller) (inventory buyer)
  if given
    then do
      basicTransfer price (balance buyer) (balance seller)
      return True
    else return False

sellItem :: Item -> Gold -> Player -> Player -> STM ()
sellItem item price buyer seller = do
  giveItem item (inventory seller) (inventory buyer)
  transfer price (balance buyer) (balance seller)

giveItem
  :: Item
  -> Inventory
  -> Inventory
  -> STM ()
giveItem item fromInv toInv = do
  fromList <- readTVar fromInv
  case removeInv item fromList of
    Nothing      -> retry
    Just newList -> do
      writeTVar fromInv newList
      readTVar toInv >>= writeTVar toInv . (item :)

transfer
  :: Gold
  -> Balance
  -> Balance
  -> STM ()
transfer qty fromBal toBal = do
  fromQty <- readTVar fromBal
  when (qty > fromQty) retry
  writeTVar fromBal (fromQty - qty)
  readTVar toBal >>= writeTVar toBal . (qty +)
