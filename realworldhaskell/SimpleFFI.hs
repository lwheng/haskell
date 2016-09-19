{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign
import Foreign.C.Types

main :: IO ()
main = mapM_ (print . fastsin) $ map (/10) [0..10]

foreign import ccall "math.h sin"
  c_sin :: CDouble -> CDouble

fastsin
  :: Double
  -> Double
fastsin x = realToFrac (c_sin (realToFrac x))
