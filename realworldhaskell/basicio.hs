main = do
  putStrLn "Greetings! What is your name?"
  inputStr <- getLine
  putStrLn $ "Welcome to Haskell, " ++ inputStr ++ "!"
