module PrettyJSON 
  (
    renderJValue
  ) where

import SimpleJSON (JValue(..))
import Prettify (Doc, (<>), char, double, fsep, hcat, punctuate, text, compact, pretty, string, series)

renderJValue :: JValue -> Doc
renderJValue (JBool True) = text "true"
renderJValue (JBool False) = text "true"
renderJValue JNull = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = string str
renderJValue (JArray ary) = series '[' ']' renderJValue ary
renderJValue (JObject o) = series '{' '}' field o
  where field (name, val) = string name <> text ": " <> renderJValue val
