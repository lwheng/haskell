-- Author: Heng Low Wee

-- | Module that converts String to the binary representation and back
module StringTransmitter (
    main
) where

import           Data.Char (ord, chr)
import           Debug.Trace

main :: IO ()
main = putStrLn "Hello World"

type Bit = Int

-- | Converts a string bits (in the reversed order!) to the integer representation
bin2Int
  :: [Bit]
  -> Int
bin2Int = foldr (\x y -> x + 2 * y) 0

-- | Converts integer to the bits representation (in the reversed order)
int2Bits
  :: Int -> [Bit]
int2Bits 0 = []
int2Bits n = n `mod` 2 : int2Bits (n `div` 2)

-- | Truncate or fill to make 8-bits
make8
  :: [Bit]
  -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

-- | Encode a String in bits
encode
  :: String
  -> [Bit]
encode = concat . map (make8 . int2Bits . ord)

-- | Chop list of bits into 8-bits
chop8
  :: [Bit]
  -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

-- | Decode bits to String
decode
  :: [Bit]
  -> String
decode = map (chr . bin2Int) . chop8

transmit
  :: String
  -> String
transmit = decode . channel . encode

channel
  :: [Bit]
  -> [Bit]
channel bits = trace (show bits) bits
