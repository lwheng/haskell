module StringCompression (
    main
) where

main :: IO ()
main = do
  s <- getLine
  putStrLn $ compressString s

compressString
  :: String
  -> String
compressString [] = []
compressString [x] = [x]
compressString xs = printSameOnes (takeWhile (== head xs) xs) ++ compressString (dropWhile (== head xs) xs)

-- ""    -> ""
-- "a"   -> "a"
-- "aa"  -> "a2"
-- "aaa" -> "a3"
printSameOnes
  :: String
  -> String
printSameOnes [] = []
printSameOnes xs = head xs : printLength (length xs)

printLength
  :: Int
  -> String
printLength 0 = ""
printLength 1 = ""
printLength i = show i
