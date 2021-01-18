module StringMingling (
    main
  , mingling
) where

main :: IO ()
main = do
  [p, q] <- lines <$> getContents
  putStrLn $ mingling p q

mingling
  :: String
  -> String
  -> String
mingling [] [] = []
mingling p  [] = p
mingling [] q  = q
mingling (p:ps) (q:qs) = p:q:(mingling ps qs)
