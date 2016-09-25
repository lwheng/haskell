module BloomFilter (
) where

import BloomFilter.Internal
import BloomFilter.Mutable  (insert, new)
import Data.Array.ST        (runSTUArray)
import Data.Array.IArray    ((!), bounds)
import Data.Word            (Word32)
import Prelude                            hiding (elem, length, notElem)

length
  :: Bloom a
  -> Int
length = fromIntegral . len

len
  :: Bloom a
  -> Word32
len = succ . snd . bounds . blmArray

elem
  :: a
  -> Bloom a
  -> Bool
elt `elem` filt = all test (blmHash filt elt)
  where
    test hash = blmArray filt ! (hash `mod` len filt)

notElem
  :: a
  -> Bloom a
  -> Bool
elt `notElem` filt = not (elt `elem` filt)

fromList
  :: (a -> [Word32])
  -> Word32
  -> [a]
  -> Bloom a
fromList hash numBits values =
  B hash $ runSTUArray $ do
    mb <- new hash numBits
    mapM_ (insert mb) values
    return (mutArray mb)
