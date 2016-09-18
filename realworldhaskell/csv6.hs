import Text.ParserCombinators.Parsec

csvFile = endBy line eol
line = sepBy cell (char ',')
cell = many (noneOf ",\n\r")
eol =   try (string "\n\r")
    <|> try (string "\r\n")
    <|>     string "\n"
    <|>     string "\r"
    <?> "end of line" -- Better error message. Without this line, it prints out all parse options

parseCSV
  :: String
  -> Either ParseError [[String]]
parseCSV input = parse csvFile "(unknown)" input
