-- Writing a Haskell module
-- To write a module, we start with module <ModuleName>
-- <Module Name> has to be same as filename
--
-- To compile this Haskell source, ghc -c <filename.hs>
-- -c option generates only object code
-- Omitting -c option will cause ghc to attempt to generate a complete executable which needs "main" defined
module SimpleJSON
  (                       -- Whatever is inside the brackets are exported
    JValue(..)            -- i.e. available for use and visible to other modules
    , getString           -- JValue(..) means export both type and all its constructors
    , getInt
    , getDouble
    , getBool
    , getObject
    , getArray
    , isNull
  ) where                 -- where, the definitions follow

data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject [(String, JValue)]
            | JArray [JValue]
            deriving (Eq, Ord, Show)
-- Creating JSON value, we need to use the distinct constructor

-- To extract the String from JString, we use pattern matching
getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing
-- Notice truncate: truncate 3.14 ==  3

getDouble (JNumber n)  = Just n
getDouble _            = Nothing

getBool (JBool b) = Just b
getBool _         = Nothing

getObject (JObject o) = Just o
getObject _           = Nothing

getArray (JArray a) = Just a
getArray _          = Nothing

isNull v = v == JNull
