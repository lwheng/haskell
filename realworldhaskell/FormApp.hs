import Control.Applicative           hiding ((<|>), many)
import Control.Monad
import Numeric
import Text.ParserCombinators.Parsec

a_hex
  :: CharParser () Char
a_hex = hexify <$> (char '%' *> hexDigit) <*> hexDigit

hexify
  :: Char
  -> Char
  -> Char
hexify a b = toEnum . fst . head . readHex $ [a, b]

a_char
  :: CharParser () Char
a_char =   oneOf urlBaseChars
       <|> (' ' <$ char '+')
       <|> a_hex

urlBaseChars =  ['a'..'z']
             ++ ['A'..'Z']
             ++ ['0'..'9']
             ++ "$-_.!*'(),"

a_pair
  :: CharParser () (String, Maybe String)
a_pair = liftM2 (,) (many1 a_char) (optionMaybe (char '=' *> many a_char))
