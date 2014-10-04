-- This function take a string and breaks on \r or \n
-- Returns a list of tokens
splitLines :: String -> [String]

splitLines [] = []
splitLines cs = let
                  (pre, suf) = break isLineTerminator cs
                -- break, is the function that splits a list into left and right
                -- Take a Bool function, return the left & right
                -- e.g. break odd [2,4,7,8,10] returns ([2,4], [7,8,10])
                -- notice the element that returns True on "odd" is on the right
                in
                  pre : case suf of
                    -- we apply splitLines recursively on the right
                    ('\r':'\n':rest) -> splitLines rest
                    ('\r':rest)      -> splitLines rest
                    ('\n':rest)      -> splitLines rest
                    _                -> []

-- This is the Bool function that "break" uses to split the string into left and right
isLineTerminator c = c == '\r' || c == '\n'

-- It is worth noting the function, lines
-- lines, though useful, splits on the '\n' character only
-- This, in general, is not safe enough especially when we read a Windows-generated file on Linux, that we will see ...\r\n
-- e.g.
-- lines "a\r\nb" returns ["a\r", "b"]

-- The reverse of splitLines, joins
-- Check out the function, unlines
fixLines :: String -> String
fixLines input = unlines (splitLines input)
