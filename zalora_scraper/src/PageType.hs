module
PageType
where

import StringTools

data PageType = Index | Category | SubCategory | Product | Brand | Blank
                deriving (Show)

regex_index = "shop\\.pc\\.index.*"
regex_cat = "shop\\.pc\\.cat.*"
regex_subcat = "shop\\.pc\\.subcat.*"
regex_product = "shop\\.pc\\.product.*"
regex_brand = "shop\\.pc\\.brand.*"

getPageType :: String -> PageType
getPageType input
    | regexTester regex_index input == True = Index
    | regexTester regex_cat input == True = Category
    | regexTester regex_subcat input == True = SubCategory
    | regexTester regex_product input == True = Product
    | regexTester regex_brand input == True = Brand
    | otherwise = Blank
