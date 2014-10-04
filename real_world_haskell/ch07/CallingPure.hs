name2reply :: String -> String
name2reply name = 
  "Pleased to meet you, " ++ name ++ ".\n" ++
  "Your name has " ++ charCount ++ " characters."
  where charCount = show (length name)


main :: IO ()
main = do
        putStrLn "Greetings again! What's your name?"
        inpStr <- getLine
        putStrLn $ name2reply inpStr

-- A function that involves IO is not pure
-- It has side effects
-- It might return different output when given the same input
--
-- A pure function always return the same value when given the same input
