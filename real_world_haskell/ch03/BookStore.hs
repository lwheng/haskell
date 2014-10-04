-- Type synonyms, a little like giving a name to the type, so it might make more sense
type ISBN = Int
type BookTitle = String
type Authors = [String]
type Review = String

type CustomerID = Int
type CustomerName = String
type CustomerAddress = [String]

type BookRecord = (BookInfo, BookReview)

type CardHolder = String
type CardNumber = String
type Address = [String]

-- Definiing new custom type
-- data <TYPE> = <TYPE CONSTRUCTOR> <.....>
-- In real world haskell, <TYPE> is usually equal to <TYPE CONSTRUCTOR>
-- Simply the TYPE and TYPE CONSTRUCTOR don't clash
data BookInfo = Book ISBN BookTitle Authors
  deriving (Show)

data MagazineInfo = Magazine Int String [String]
  deriving (Show)

-- Record Syntax
data Customer = Customer {
   customerID :: CustomerID
  ,customerName :: CustomerName
  ,customerAddress :: CustomerAddress
} deriving (Show)

-- -- The above actually represents the following:
-- data Customer = Customer Int String [String]
--   deriving (Show)
-- 
-- customerID :: Customer -> Int
-- customerID (Customer id _ _) = id
-- 
-- customerName :: Customer -> String
-- customerName (Customer _ name _) = name
-- 
-- customerAddress :: Customer -> [String]
-- customerAddress (Customer _ _ address) = address

-- Notice how we define this type using pre-defined type synonyms
data BookReview = BookReview BookInfo CustomerID Review

data BillingInfo = CreditCard CardNumber CardHolder Address
                  | CashOnDelivery
                  | Invoice CustomerID
                  deriving (Show)

-- Just simple pattern matching to retrieve
bookISBN (Book isbn _ _) = isbn
bookTitle (Book _ booktitle _) = booktitle
bookAuthors (Book _ _ authors) = authors

myInfo = Book 1234 "Hello World" ["Author A", "Author B"]

myCustomer = Customer 3300 "John Smith" ["1", "Infinity Loop"]

-- Using Record Syntax you can do this:
-- customerID myCustomer => 3300

-- Using Record Syntax makes it easier for you to create value
-- Note that the order didn't matter when creating value using Record Syntax
customer2 = Customer {
   customerName = "Jane Doe"
  ,customerAddress = ["Lane 1", "Lane 2"]
  ,customerID = 991
}
