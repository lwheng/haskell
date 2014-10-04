main = do
        -- putStrLn is a function that takes a String and outputs an IO action
        -- check it out with ghci: :type putStrLn
        putStrLn "Greetings! What's your name?"
        inpStr <- getLine
        -- the "<-" syntax is like extracting whatever the IO action have
        -- getLine is: getLine :: IO String
        -- getLine's syntax kinda mean it is storing an IO action, when performed, returns a String
        -- so we extract the String, or unwrap the String
        putStrLn $ "Welcome to Haskell, " ++ inpStr ++ "!"
        -- the $ sign is something new
        -- what it does is it wraps whatever is on the right hand side with brackets
        -- so...
        -- putStrLn (show (1 + 1))
        -- is the same as...
        -- putStrLn $ show (1 + 1)
        -- putStrLn (show $ 1 + 1)
        -- putStrLn $ show $ 1 + 1
        --
        -- "IO ()" is an IO action
        -- () is empty tuple (pronounced "unit"), meaning there's no return value for this IO action
        -- kinda like "void" in Java or C
        --
        -- The thing about IO action is, if stored, nothing happens
        -- an IO action is only executed with its parent IO action is executed
        -- yes, IO actions can be glued together to form bigger IO actions
        -- IO action in IO action in IO action........
        -- 
        -- In this example, "main" is the parent IO action with the type "IO ()"
        --
        -- the "do" syntax is like imperative programming
        -- line by line, action by action
