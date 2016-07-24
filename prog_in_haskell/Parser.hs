module Parser (
    main
) where

main :: IO ()
main = return ()

type Parser a = String -> [(a, String)]

-- In the book, this function is named return
-- Always succeeds with the provided v
initParser
  :: a
  -> Parser a
initParser v = \inp -> [(v, inp)]

-- Always fail regardless the input
failure :: Parser a
failure = \_ -> []

-- Parses one character
item :: Parser Char
item = \inp -> case inp of
                 []     -> []
                 (x:xs) -> [(x, xs)]

-- To call a Parser on an input
parse
  :: Parser a
  -> String
  -> [(a, String)]
parse p inp = p inp

-- Combine parsers
(>>=)
  :: Parser a
  -> (a -> Parser b)
  -> Parser b
p >>= f = \inp -> case parse p inp of
                    []         -> []
                    [(v, out)] -> parse (f v) out

-- An example parser that consumes 3 characters, discards the 2nd and returns the first and third in a tuple
-- TODO this doesn't work without monads?
-- p :: Parser (Char, Char)
-- p = do
--       x <- item
--       item
--       y <- item
--       initParser (x,y)

(+++)
  :: Parser a
  -> Parser a
  -> Parser a
p +++ q = \inp -> case parse p inp of
                    []        -> parse q inp
                    [(v,out)] -> [(v,out)]

-- TODO does not compile
-- sat
--   :: (Char -> Bool)
--   -> Parser Char
-- sat p = do x <- item
--            if p x
--              then initParser x
--              else failure
