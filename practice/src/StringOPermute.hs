module StringOPermute (
    main
) where

import Control.Monad (forM_)

main :: IO ()
main = do
  t <- (read <$> getLine) :: IO Int

  forM_ [1..t] $ \_ -> do
    s <- getLine
    putStrLn $ permute s

permute
  :: String
  -> String
permute [] = []
permute [x] = [x]
permute (x:y:ys) = y:x:(permute ys)
