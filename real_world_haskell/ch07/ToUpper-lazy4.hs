import Data.Char(toUpper)

-- interact :: (String -> String) -> IO ()
main = interact (map toUpper)

-- run this with:
-- runghc <filename.hs> < input.txt
-- or
-- runghc <filename.hs> 
-- for interactive mode
