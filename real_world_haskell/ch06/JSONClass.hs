{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module JSONClass
(
  JAry(fromJAry)
  , jary
)
where

-- import SimpleJSON
import Control.Arrow (second)
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            -- | JObject [(String, JValue)]
            | JObject (JObj JValue)
            -- | JArray [JValue]
            | JArray (JAry JValue)
            deriving (Eq, Ord, Show)

type JSONError = String

newtype JAry a = JAry {
  fromJAry :: [a]
} deriving (Eq, Ord, Show)

newtype JObj a = JObj {
  fromJObj :: [(String, a)]
} deriving (Eq, Ord, Show)

-- usual practice: to define a function that applies the constructor
-- so that we don't expose the data constructor in order to keep the details of type abstract
-- a little like interface
jary :: [a] -> JAry a
jary = JAry

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

instance JSON Bool where
  toJValue = JBool
  fromJValue (JBool b) = Right b
  fromJValue _ = Left "Not a JBool"
-- Notice in the class JSON we define the type signature to return a
-- Either
-- So for the instances we must return either Left or Right
-- in the case of JSON Bool,
-- we return the value if it is a JBool, otherwise a JSONError (i.e. String)

-- because String is essentially [Char], a synonym
-- We cannot create a instance of it without relaxing a restriction
-- The comment added at the first line of this file does that
-- with that we can compile this
instance JSON String where
  toJValue = JString
  fromJValue (JString s) = Right s
  fromJValue _ = Left "Not a JString"

doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "Not a JNumber"

instance JSON Int where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Integer where
  toJValue = JNumber . realToFrac
  fromJValue = doubleToJValue round

instance JSON Double where
  toJValue = JNumber
  fromJValue = doubleToJValue id
-- we can add new instances anywhere in our code, not restricted to the module where we defined the typeclass
-- this notion is "open world assumption"
-- if otherwise, "closed world"

-- here we write instance for JAry and JObj
-- RECALL: take jaryFromJValue for example
-- For all types "a", so long "a" is a instance of JSON, jaryFromJValue take a JValue as parameter
-- and return an Either as output
jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryToJValue :: (JSON a) => JAry a -> JValue
instance (JSON a) => JSON (JAry a) where
  toJValue = jaryToJValue
  fromJValue = jaryFromJValue

-- So to convert JAry a  to a JValue...
listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

-- Wrapping [JValue] to become JAry JValue
jvaluesToJAry :: [JValue] -> JAry JValue
jvaluesToJAry = JAry

jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray
-- This is just from the constructor

jaryToJValue = JArray . JAry . map toJValue . fromJAry

jaryFromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _ = Left "Not a JSON Array"

-- whenRight takes a function, applies it to its 2nd argument only if it was created with the Right constructor
whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a) = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers _ _ = Right []

instance (JSON a) => JSON (JObj a) where
  toJValue = JObject . JObj . map (second toJValue) . fromJObj
  fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
    where unwrap (k,v) = whenRight ((,) k) (fromJValue v)
  fromJValue _ = Left "Not a JSON Object"

-- (,) is a function that takes 2 arguments and returns a tuple of them
-- (,,) takes 3
-- and so on
