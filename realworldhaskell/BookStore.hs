-- Constructing a new data type
-- data <type constructor> = <value constructor> <components...>
data BookInfo = Book Int String [String]
                 deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)
-- Contructing a value
myInfo = Book 9123123 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

-- Constructing type synonyms
type CustomerID = Int
type ReviewBody = String
-- <type constructor> and <value constructor> can be the same
data BookReview = BookReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

-- Multiple value constructors
data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

-- Record Syntax
-- Usually very useful for data type with large structures
data Customer = Customer {
    customerID :: CustomerID
  , customerName :: String
  , customerAddress :: Address
  } deriving (Show)
