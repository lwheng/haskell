module PNM
where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

data Greymap = Greymap {
    greyWidth :: Int
  , greyHeight :: Int
  , greyMax :: Int
  , greyData :: L.ByteString
} deriving (Eq)

instance Show Greymap where
  show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
  case matchHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s1 -> case getNat s1 of
      Nothing -> Nothing
      Just (width, s2) -> case getNat s2 of
        Nothing -> Nothing
        Just (height, s3) -> case getNat (L8.dropWhile isSpace s3) of
          Nothing -> Nothing
          Just (maxGrey, s4)
            | maxGrey > 255 -> Nothing
            | otherwise -> case getBytes 1 s4 of
                             Nothing -> Nothing
                             Just (_, s5) -> case getBytes (width*height) s5 of
                                               Nothing -> Nothing
                                               Just (bitmap, s6) -> Just (Greymap width height maxGrey bitmap, s6)
-- As one might see, the code just keeps going to the right
-- We are repeating a lot on: Checking whether it is Nothing / Just a
-- All these "case" are boilerplate code that should gotten rid of

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
  | prefix `L8.isPrefixOf` str
      = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
  | otherwise
      = Nothing

-- nat means Natural Number
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
             Nothing -> Nothing
             Just (num, rest)
               | num <= 0 -> Nothing
               | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count = fromIntegral n
                     both@(prefix,_) = L.splitAt count str
                 in
                   if L.length prefix < count
                     then Nothing
                     else Just both

-- This function is created based on the pattern we can observe
-- in parseP5
-- We check whether it is Nothing. If yes, we pass it on
-- If not, we unwrap it and let the following function act on it
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v
-- i.e. on the left side of >>? is a Maybe value
-- on the right side is a function that returns a Maybe value
-- hence we can chain them
-- We only need to add the brackets when we define its type signature

parseP5_take2 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5_take2 s =
        matchHeader (L8.pack "P5") s -- outputs a Maybe, pass it on with
                                     -- our >>? operator
  >>? \s -> skipSpace ((), s)
  >>? (getNat . snd)
  >>? skipSpace
  >>? \(width, s) -> getNat s
  >>? skipSpace
  >>? \(height, s) -> getNat s
  >>? \(maxGrey, s) -> getBytes 1 s
  >>? (getBytes (width * height) . snd)
  >>? \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)
-- Notice we are passing a 2-tuple throughout and we use pattern matching to extract
-- What is we want to pass a 3-tuple? Modify each line?

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)
