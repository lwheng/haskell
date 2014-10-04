str2message :: String -> String
str2message input = "Data: " ++ input

str2action :: String -> IO ()
str2action = putStrLn . str2message

numbers :: [Int]
numbers = [1..10]

main = do
        str2action "Start of the program"
        mapM_ (str2action . show) numbers
        str2action "Done!"

-- mapM_ is a special "map" for IO actions
-- similarly it takes a function and a list, except that this function is an IO action that is to be performed for each item in the list
-- it combines the list of IO actions into one and executes them
--
-- mapM is another special "map"
-- the difference is mapM don't execute them, but stores them instead
--
-- rule of thumb: whenever you see "M" in the function name, think IO
-- end with "_": usually means they discard their results
