module
TagSoupHelper
where

import Text.HTML.TagSoup
import StringTools

getTags :: String -> [Tag String]
getTags = parseTags

-- This function retrieves the [Attribute] of a open-tag
getAttributeList :: Tag String -> [Attribute String]
getAttributeList (TagOpen a xs) = xs

filterTagText :: [Tag String] -> [Tag String]
filterTagText = filter (\s -> isTagText s)

filterTagOpen :: String -> [Tag String] -> [Tag String]
filterTagOpen tagName = filter (isTagOpenName tagName)

filterIsDataLayer :: [Tag String] -> [Tag String]
filterIsDataLayer = filter (\(TagText str) -> str `startsWithThis` dataLayer)

getTagTextValue :: Tag String -> String
getTagTextValue (TagText str) = replaceString "'" "\"" $ replaceString "\n" "" str

-- A open-tag is like "<li>", "<ul>", "<a href......."
-- This function takes the Attribute list of a open-tag and 
-- extracts the value of the "id" attribute
extractSKU :: [Attribute String] -> String
extractSKU [] = ""
extractSKU ((x,y):xs) | x == "id" = y
                      | otherwise = extractSKU xs
