data CannotShow = CannotShow
  deriving (Show)

data CannotDeriveShow = CannotDeriveShow CannotShow
  deriving (Show)

data OK = OK

instance Show OK where
  show _ = "OK"

data ThisWorks = ThisWorks OK
  deriving (Show)
-- We are asking Haskell to deriving Show for us for ThisWorks
-- For it to work, all the functions all by ThisWorks must also
-- be instances of Show
