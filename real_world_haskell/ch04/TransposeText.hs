import System.Environment (getArgs)
import Data.List
splitLines :: String -> [String]
splitLines [] = []
splitLines cs = let
                  (pre, suf) = break isLineTerminator cs
                in
                  pre : case suf of
                    ('\r':'\n':rest) -> splitLines rest
                    ('\r':rest)      -> splitLines rest
                    ('\n':rest)      -> splitLines rest
                    _                -> []
isLineTerminator c = c == '\r' || c == '\n'

join :: [String] -> String
join (x:xs) = x ++ "\n" ++ join xs
join [] = []

betterjoin :: [String] -> String
betterjoin [] = []
betterjoin xs = concat (map (\x -> x ++ "\n") xs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        myFunction = betterjoin . transpose . splitLines
-- Spent quite a lot of time trying to write a transpose function
-- In the end found out there exists one already.
