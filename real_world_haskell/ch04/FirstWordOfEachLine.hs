import System.Environment (getArgs)

-- We use splitLines as defined previously
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

-- I introduce a helper function that helps me join list of words
-- with newline character
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

        myFunction = betterjoin . (map (head . words)) . splitLines
-- So, myFunction
-- First apply `splitLines` to split input into lines
-- For each line in the list of lines, apply (head . words). Now we get the all the first words of each lines in a list
-- Finally we join them using `join` so the output will look better
