import System.Environment (getArgs)

-- do, introduces a block of actions
-- <-, an assignment within the do block
interactWith function inputFile outputFile = do
  -- read inputFile using readFile into input
  input <- readFile inputFile
  -- apply function to input, then write to outputFile using writeFile
  writeFile outputFile (function input)

-- When we run the program is runs "main" first
main = mainWith myFunction
  where mainWith function = do
          -- getArgs is the imported function that help us get the command line arguments
          args <- getArgs
          case args of
            -- Match pattern for exactly 2 arguments
            -- If match, then we apply the interactWith function
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        -- Notice myFunction is the function applied onto our input
        -- As for now, we assign myFunction = id
        -- id is like an identity function, return whatever is the input
        -- i.e. we do nothing to the input for now
        myFunction = id
