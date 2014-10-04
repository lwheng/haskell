import Data.Char(toUpper)

main = do
        interact ((++) "Your data, in uppercase, is:\n\n" .
                  (map toUpper))

-- run this with: runghc <filename.hs> < input.txt
