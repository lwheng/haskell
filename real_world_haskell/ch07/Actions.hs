str2action :: String -> IO ()
str2action input = putStrLn ("Data: " ++ input)

list2action :: [String] -> [IO ()]
list2action = map str2action
-- by having list2action, we are storing the actions and not executing them immediately

numbers :: [Int]
numbers = [1..10]

strings :: [String]
strings = map show numbers

actions :: [IO ()]
actions = list2action strings

printitall :: IO ()
printitall = runall actions

-- Takes a list of actions and execute each of them in turn
runall :: [IO ()] -> IO ()
runall [] = return ()
runall (x:xs) = do
                  x -- so we can see that just by invoking the saved action, we are running them
                  runall xs

main :: IO ()
main = do
        str2action "Start of the program"
        printitall
        str2action "Done!"
