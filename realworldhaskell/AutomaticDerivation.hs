data CannotShow = CannotShow
-- Either we derive the Show, or we define the instance ourselves

data CannotDeriveShow = CannotDeriveShow CannotShow
  deriving (Show)
