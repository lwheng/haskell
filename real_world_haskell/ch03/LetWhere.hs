bar = let b = 2
          c = True
      in let a = b
         in (a, c)
-- a in only accessible in the inner LET

foo = x
      where x = y
                where y = 4
-- this demos using WHERE
