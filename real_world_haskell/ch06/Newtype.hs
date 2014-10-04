data DataInt = D Int
  deriving (Eq, Ord, Show)

data Param a b = Param (Either a b)
  deriving (Eq, Ord, Show)

newtype NewtypeInt = N Int
  deriving (Eq, Ord, Show)

type ErrorString = String

-- newtype is another way to define a new type
-- The purpose is to rename an existing type, giving it a distinct identity
