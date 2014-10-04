-- This is just a simple microbenchmark that read a text file full of numbers and prints their sum
-- use it: runghc <filename.hs> < input.txt
--
-- However it is not efficient because:
-- String is essentially [Char]
-- each Char is allocated memory individually -> bookkeeping overhead
-- memory consumption; performance issues
--
-- Thus, bytestring

main = do
        contents <- getContents
        print (sumFile contents)
       where sumFile = sum . map read . words
