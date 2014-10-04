module
StringTools
where

import Data.Text
import qualified Data.List as List
import Data.Char
import Text.Regex.Posix

dataLayer = "dataLayer"
shopPc = "wt.contentId = \"shop.pc"

removeSpace :: String -> String
removeSpace = replaceString " " ""

replaceString :: String -> String -> String -> String
replaceString search replacement input =
    unpack $ replace (pack search) (pack replacement) (pack input)

regexMatcher :: String -> String -> (String, String, String)
regexMatcher regex input = input =~ regex :: (String, String, String)

regexTester :: String -> String -> Bool
regexTester regex input = input =~ regex :: Bool

isShopPC :: String -> Bool
isShopPC s = s `startsWithThis` shopPc

startsWithThis :: String -> String -> Bool
input `startsWithThis` prefix = prefix `List.isPrefixOf` (List.dropWhile isSpace input)

getDataLayer :: String -> String
getDataLayer input = case regexMatcher "dataLayer = " input of
                       (_, _, after) ->
                         case regexMatcher ";" after of
                           (before, _, _) -> before

getShopPC :: String -> String
getShopPC input = case regexMatcher "shop\\.pc\\..*\" " input of
                    (_, match, _) -> List.init $ List.init match

filterMatching :: String -> [String] -> [String]
filterMatching regex = List.filter (\s -> s =~ regex :: Bool)
