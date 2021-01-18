module PrefixCompression (
    main
) where

main :: IO ()
main = do
  [x, y] <- lines <$> getContents

  let
    (p, x', y') = prefixCompression x y

  printer p
  printer x'
  printer y'

printer
  :: String
  -> IO ()
printer s = putStrLn $ show (length s) ++ " " ++ s

prefixCompression
  :: String
  -> String
  -> (String, String, String)
prefixCompression x y = (p, x', y')
  where
    zipped = zip x y
    p = map fst $ takeWhile (\(x1, y1) -> x1 == y1) zipped
    x' = drop (length p) x
    y' = drop (length p) y
