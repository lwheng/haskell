import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace, chr, isDigit)
import Data.Int (Int64)
import Data.Word
import Control.Applicative

import PNM -- if you want to do this, the source file must have the "module" header defined, telling it which functions are exposed

data ParseState = ParseState {
                    string :: L.ByteString
                  , offset :: Int64
} deriving (Show)
-- To create a ParseState:
-- let a = ParseState { string = L8.pack "Hello", offset = 123 }
-- or 
-- let a = ParseState (L8.pack "Hello") 123
--
-- In PNM.hs we used pattern matching in order to extract
-- Now with record syntax we don't need to do that.
-- In fact, record syntax means we can add another arguments in the constractor when we want to add another information, and still be able to access it using the accessor
--
-- So now, instead of (Greymap, L.ByteString),
-- we have (a, ParseState)

simpleParse :: ParseState -> (a, ParseState)
simpleParse = undefined

betterParse :: ParseState -> Either String (a, ParseState)
betterParse = undefined

-- We can hide the actual implementation of the parser by
-- using a "newtype"
-- To actually start parsing, we apply the "runParse" accessor
-- imagine:
-- Parse Int. So it translate to a function:
-- runParse :: ParseState -> Either String (Int, ParseState)
-- i.e. you can tell it what you type want to parse
newtype Parse a = Parse {
  runParse :: ParseState -> Either String (a, ParseState)
}

-- the identity
-- This function we leave the ParseState untouched,
-- and uses the argument in the result
identity :: a -> Parse a
identity a = Parse (\s -> Right (a, s))
-- Here "s" is ParseState


parse :: Parse a -> L.ByteString -> Either String a
parse parser initState
  = case runParse parser (ParseState initState 0) of
      Left err -> Left err
      Right (result, _) -> Right result

modifyOffset :: ParseState -> Int64 -> ParseState
modifyOffset initState newOffset = initState { offset = newOffset}

-- import the Word8 type from Data.Word
parseByte :: Parse Word8
parseByte = 
    -- notice this style of anonymous function
    -- "hanging lambda"
    -- the body of the function is on the next line
    -- main purpose: make room for the function
    getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing -> 
        bail "no more input"
      Just (byte, remainder) ->
        putState newState ==> \_ ->
        identity byte
        where newState = initState { string = remainder, offset = newOffset}
              newOffset = offset initState + 1

-- This function extracts the ParseState
getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

-- This function puts the ParseState
putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))

bail :: String -> Parse a
bail err = Parse $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err

-- A little like function composition
-- Apply first, then apply second on the results of the first
(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser = Parse chainedParser
    where chainedParser initState =
            case runParse firstParser initState of
                Left errMessage -> Left errMessage
                Right (firstResult, newState) ->
                    runParse (secondParser firstResult) newState

-- Functor instance for Parse
instance Functor Parse where
    fmap f parser = parser ==> \result ->
                    identity (f result)

-- Making use of the power of Functors, we can easily write a parseChar
-- We only need to first convert Word8 to Char
w2c :: Word8 -> Char
w2c = chr . fromIntegral

parseChar :: Parse Char
parseChar = w2c <$> parseByte
-- Wow moment here. simply lifted parseByte to a parseChar

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

peekChar :: Parse (Maybe Char)
peekChar = fmap w2c <$> peekByte
-- Another Wow moment


parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
                 then parseByte ==> \b ->
                      (b:) <$> parseWhile p
                 else identity []

parseWhileVerbose p =
    peekByte ==> \mc ->
    case mc of
      Nothing -> identity []
      Just c | p c ->
                 parseByte ==> \b ->
                 parseWhileVerbose p ==> \bs ->
                 identity (b:bs)
             | otherwise ->
                 identity []

parseRawPGM =
    parseWhileWith w2c notWhite ==> \header -> skipSpaces ==>&
    assert (header == "P5") "invalid raw header" ==>&
    parseNat ==> \width -> skipSpaces ==>&
    parseNat ==> \height -> skipSpaces ==>&
    parseNat ==> \maxGrey ->
    parseByte ==>&
    parseBytes (width * height) ==> \bitmap ->
    identity (Greymap width height maxGrey bitmap)
  where notWhite = (`notElem` " \r\n\t")

parseWhileWith :: (Word8 -> a) -> (a -> Bool) -> Parse [a]
parseWhileWith f p = fmap f <$> parseWhile (p . f)

parseNat :: Parse Int
parseNat = parseWhileWith w2c isDigit ==> \digits ->
           if null digits
           then bail "no more input"
           else let n = read digits
                in if n < 0
                   then bail "integer overflow"
                   else identity n

(==>&) :: Parse a -> Parse b -> Parse b
p ==>& f = p ==> \_ -> f

skipSpaces :: Parse ()
skipSpaces = parseWhileWith w2c isSpace ==>& identity ()

assert :: Bool -> String -> Parse ()
assert True  _   = identity ()
assert False err = bail err

parseBytes :: Int -> Parse L.ByteString
parseBytes n =
    getState ==> \st ->
    let n' = fromIntegral n
        (h, t) = L.splitAt n' (string st)
        st' = st { offset = offset st + L.length h, string = t }
    in putState st' ==>&
       assert (L.length h == n') "end of input" ==>&
       identity h
