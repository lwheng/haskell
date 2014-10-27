name2reply :: String -> String
name2reply name = 
  "Pleased to meet you, " ++ name ++ ".\n" ++
  "Your name contains " ++ charcount ++ " characters."
  where charcount = show (length name)

main :: IO ()
main = do
  putStrLn "Greetings, what is your name?"
  inputStr <- getLine
  let outputStr = name2reply inputStr
  putStrLn outputStr
