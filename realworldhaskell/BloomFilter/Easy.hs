module BloomFilter.Easy (
    easyList
) where

import           Data.Hashable
import qualified BloomFilter.Internal as B

easyList
  :: (Hashable a)
  => Double
  -> [a]
  -> Either String (B.Bloom a)
easyList = undefined
