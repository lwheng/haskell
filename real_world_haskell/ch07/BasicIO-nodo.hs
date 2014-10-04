main = putStrLn "Greetings! What is your name?" >>
       -- the ">>" syntax is for sequencing, and it replaces the need for a "do" at the beginning
       -- it means "continue..."
       getLine >>=
       -- the ">>=" is similar, except that it passes whatever result to the next line
       -- getLine returns a line of text
       (\inputStr -> putStrLn $ "Welcome to Haskell, " ++ inputStr ++ "!")
