module
ZaloraProduct
where 

import StringTools
import Text.JSON

data ZaloraProduct = ZaloraProduct {
                genderCategory :: Maybe String
              , productCategory :: Maybe String
              , productSubcategory :: Maybe String
              , brand :: Maybe String
              , productName :: Maybe String
              , productSKU :: Maybe String
              , landingPageId :: Maybe String
              , customerType :: Maybe String
              , genderUser :: Maybe String
              , paymentType :: Maybe String
              , numOfProducts :: Maybe String
              , promotionCode :: Maybe String
              , countryProvince :: Maybe String
              , yearOfBirth :: Maybe String
              , brandCategory :: Maybe String
              , userId :: Maybe String
              , productPrice :: Maybe String
              , discount :: Maybe String
              , orderNr :: Maybe String
              , revenue :: Maybe String
} deriving (Show)

getZaloraProduct :: Result ZaloraProduct -> ZaloraProduct
getZaloraProduct = (\(Ok x) -> x)

makeZaloraProduct :: JSObject JSValue -> Result ZaloraProduct
makeZaloraProduct zalora_product = let (!) = flip valFromObj in do
    genderCategory <- zalora_product ! "Gender_Category"
    productCategory <- zalora_product ! "Product_Category"
    productSubcategory <- zalora_product ! "Product_Subcategory"
    brand <- zalora_product ! "Brand"
    productName <- zalora_product ! "Product_Name"
    productSKU <- zalora_product ! "Product_SKU"
    landingPageId <- zalora_product ! "Landingpage_ID"
    customerType <- zalora_product ! "Customer_Type"
    genderUser <- zalora_product ! "Gender_User"
    paymentType <- zalora_product ! "Payment_Type"
    numOfProducts <- zalora_product ! "Number_of_Products"
    promotionCode <- zalora_product ! "Promotion_Code"
    countryProvince <- zalora_product ! "Country-Province"
    yearOfBirth <- zalora_product ! "Year_of_Birth"
    brandCategory <- zalora_product ! "Brand_Category"
    userId <- zalora_product ! "User_ID"
    productPrice <- zalora_product ! "Product_Price"
    discount <- zalora_product ! "Discount_%"
    orderNr <- zalora_product ! "OrderNr"
    revenue <- zalora_product ! "Revenue"
    Ok ZaloraProduct {
        genderCategory = setValue genderCategory
      , productCategory = setValue productCategory
      , productSubcategory = setValue productSubcategory
      , brand = setValue brand
      , productName = setValue productName
      , productSKU = setValue productSKU
      , landingPageId = setValue landingPageId
      , customerType = setValue customerType
      , genderUser = setValue genderUser
      , paymentType = setValue paymentType
      , numOfProducts = setValue numOfProducts
      , promotionCode = setValue promotionCode
      , countryProvince = setValue countryProvince
      , yearOfBirth = setValue yearOfBirth
      , brandCategory = setValue brandCategory
      , userId = setValue userId
      , productPrice = setValue productPrice
      , discount = setValue discount
      , orderNr = setValue orderNr
      , revenue = setValue revenue
    }

setValue :: String -> Maybe String
setValue input
    | length input == 0 = Nothing
    | otherwise = Just input

string2ZaloraJSON :: String -> (JSObject JSValue)
string2ZaloraJSON input = case decode (getDataLayer input) :: Result [JSObject JSValue]
                            of (Ok x) -> x !! 0
