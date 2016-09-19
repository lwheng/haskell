{-# LINE 1 "Regex.hsc" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "Regex.hsc" #-}

module Regex where

import Foreign
import Foreign.C.Types


{-# LINE 9 "Regex.hsc" #-}

newtype PCREOption = PCREOption {
                                  unPCREOption :: CInt
                                } deriving (Eq,Show)

caseless        :: PCREOption
caseless        = PCREOption 1
dollar_endonly  :: PCREOption
dollar_endonly  = PCREOption 32
dotall          :: PCREOption
dotall          = PCREOption 4

{-# LINE 19 "Regex.hsc" #-}

combineOptions
  :: [PCREOption]
  -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0
