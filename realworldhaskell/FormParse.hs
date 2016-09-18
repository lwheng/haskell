import Control.Monad
import Numeric
import Text.ParserCombinators.Parsec

p_query
  :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

p_pair
  :: CharParser () (String, Maybe String)
p_pair = do
  name <- many1 p_char
  val  <- optionMaybe (char '=' >> many p_char)
  return (name, val)

-- Applicative style of writing p_pair
p_pair_app1
  :: CharParser () (String, Maybe String)
p_pair_app1 = liftM2 (,) (many1 p_char) (optionMaybe (char '=' >> many p_char))

p_char
  :: CharParser () Char
p_char =   oneOf urlBaseChars
       <|> (char '+' >> return ' ')
       <|> p_hex

urlBaseChars =  ['a'..'z']
             ++ ['A'..'Z']
             ++ ['0'..'9']
             ++ "$-_.!*'(),"

p_hex
  :: CharParser () Char
p_hex = do
  char '%'
  a <- hexDigit
  b <- hexDigit
  let
    ((d, _) : _) = readHex [a,b]
  return . toEnum $ d
