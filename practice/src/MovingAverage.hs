module MovingAverage (
    add
  , compute
  , printContainer
  , Container (..)
) where

data Container = Container
                   {
                     cSize :: Int
                   , cElements :: [Double] -- Numbers are stored in reverse
                   }

add
  :: Double
  -> Container
  -> Container
add i c = Container (cSize c) $ take (cSize c) $ (i : cElements c)

compute
  :: Container
  -> Maybe Double
compute c = if (cSize c) == length (cElements c)
              then Just $ sum (cElements c) / (fromIntegral $ cSize c)
              else Nothing

printContainer
  :: Container
  -> IO ()
printContainer c = print $ cElements c
