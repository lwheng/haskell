import System.Environment (getArgs)

-- ghc --make InteractWith
-- This will compile the file

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error: exactly 2 arguments needed"
        -- replace "id" with the name of our function below
        myFunction = id
