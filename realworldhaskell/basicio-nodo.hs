main =
  putStrLn "Greetings! What is your name?" >> 
  getLine >>=
  (\inputStr -> putStrLn $ "Welcome to Haskell, " ++ inputStr ++ "!")
