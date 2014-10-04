import HTTPHelper
import StringTools
import ZaloraProduct
import TagSoupHelper
import PageType
import Data.Maybe
import Data.List

root = "http://www.zalora.sg"
cat_url = "http://www.zalora.sg/women/shoes/"
subcat_url = "http://www.zalora.sg/women/shoes/flats/"
sku_regex = "[A-Z]{2}[0-9]{3}[A-Z]{2}[0-9]{2}[A-Z]{5}"
product_url = "http://www.zalora.sg/Ultragirl-Ballet-Flats-116649.html"

women_or_men = "\\/(women|men)"

{-
 - shop.pc.index
 - shop.pc.index.women
 - shop.pc.cat.women%2Fshoes
 - shop.pc.subcat.women%2Fshoes%2Fflats
 - shop.pc.product.Ultragirl-Ballet-Flats-116649.html
-}

-- Need to fix this in the future?
-- Function body is too long
processProductPage :: String -> ZaloraProduct
processProductPage rawPage = getZaloraProduct .
                             makeZaloraProduct $ 
                             string2ZaloraJSON $ 
                             getTagTextValue $ 
                             head $ 
                             filterIsDataLayer . 
                             filterTagText $
                             getTags rawPage

scrapeProductPage :: String -> (String, ZaloraProduct)
scrapeProductPage rawPage = (getShopPC rawPage, processProductPage rawPage)

{-
-- This scrapes a category page for SKUs
mainCategory = do
         rawPage <- openURL url -- first get the page in raw html
         let tags = getTags rawPage -- use TagSoup to parse and get the page in Tag-s
         let filtered = filterTagOpen "li" tags -- filter only <li>
         -- For each <li> tag, get its attributes, extract its "id"'s value
         -- Then filter by regex matching
         let output = filterMatching sku_regex $ map (extractSKU . getAttributeList) filtered
         return output
-}

{-
-- Scrapes a product page for dataLayer, returns a ZaloraProduct
mainProduct = do
        rawPage <- openURL product_url
        return $ scrapeProductPage rawPage
-}

-- This function is also a bit too long
visitIndex :: String -> [String]
visitIndex = map (\s -> root ++ s) . 
             nub .
             filter (\s -> regexTester women_or_men s) . 
             map (\(Just x) -> x) . 
             filter (isJust) . 
             map (lookup "href") . 
             map getAttributeList . 
             filterTagOpen "a" . 
             getTags

visitCat :: String -> [String]
visitCat = filter (\s -> not $ "sort" `isInfixOf` s) . 
           visitIndex

visitSubcat :: String -> [String]
visitSubcat = map (\s -> root ++ s) .
              nub .
              filter (\s -> ".html" `isSuffixOf` s) .
              map (\(Just x) -> x) .
              filter (isJust) .
              map (lookup "href") .
              filter (\s -> lookup "class" s == Just "itm-link") .
              map getAttributeList .
              filterTagOpen "a" .
              getTags

crawl :: [String] -> [String] -> IO [(String, ZaloraProduct)]
crawl visited non_visited = crawler visited non_visited []
    where crawler visited_pages non_visited_pages accum
              | length non_visited_pages == 0 = return accum
              | otherwise = 
                  if (head non_visited_pages) `elem` visited_pages
                    then crawler visited_pages (tail non_visited_pages) accum
                    else do
                      let url = removeSpace $ head non_visited_pages
                      putStrLn url
                      putStrLn $ "len(visited_pages) = " ++ 
                                 (show (length visited_pages)) ++ 
                                 ", len(non_visited_pages) = " ++
                                 (show (length non_visited_pages)) ++
                                 ", len(accum) = " ++
                                 (show (length accum))
                      rawPage <- openURL url
                      case getPageType rawPage of
                        Index -> crawler (visited_pages ++ [url])
                                         ((drop 1 non_visited_pages) ++ (visitIndex rawPage))
                                         accum
                        Category -> crawler (visited_pages ++ [url])
                                            ((drop 1 non_visited_pages) ++ (visitCat rawPage))
                                            accum
                        SubCategory -> crawler (visited_pages ++ [url])
                                               ((drop 1 non_visited_pages) ++ (visitSubcat rawPage))
                                               accum
                        Brand -> crawler (visited_pages ++ [url])
                                         ((drop 1 non_visited_pages) ++ (visitSubcat rawPage))
                                         accum
                        Product -> crawler (visited_pages ++ [url])
                                           (drop 1 non_visited_pages)
                                           (accum ++ [scrapeProductPage rawPage])
                        Blank -> crawler (visited_pages ++ [url])
                                         (drop 1 non_visited_pages)
                                         accum

begin = crawl [] [root]
testCat = crawl [] [subcat_url]
