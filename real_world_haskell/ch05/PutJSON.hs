module PutJSON where

import Data.List (intercalate)
import SimpleJSON

-- List of functions to render each of the JValue
-- Notice these functions are pure i.e. no IO
renderJValue :: JValue -> String
renderJValue (JString s)    = show s
renderJValue (JNumber n)    = show n
renderJValue (JBool True)   = "true"
renderJValue (JBool False)  = "false"
renderJValue JNull          = "null"
renderJValue (JObject o) = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate ", " (map renderPair ps)
        renderPair (k,v) = show k ++ ": " ++ renderJValue v
renderJValue (JArray a) = "[" ++ values a ++ "]"
  where values [] = ""
        values vs = intercalate ", " (map renderJValue vs)

-- Separating pure code from code that performs IO is good
putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)
