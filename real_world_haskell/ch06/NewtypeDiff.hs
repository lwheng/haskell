data TwoFields = TwoFields Int Int

newtype Okay = ExactlyOne Int

newtype Param a b = Param (Either a b)

newtype Record = Record {
  getInt :: Int
}

-- all the above one compiles
-- the following don't

-- newtype need exactly one field
newtype TooFew = TooFew

-- too many fields
newtype TooManyFields = TooManyFields Int Int

-- newtype can only have one constructor
newtype TooManyCtors = Bad Int | Worse Int

-- advantage of "data"
-- more than one field, more than one constructor
-- disadvantage of "data"
-- bookkeeping cost at runtime, need to check which constructor was used to create the type
--
-- advantage of "newtype"
-- no bookkeeping cost
-- disadvantage of "newtype"
-- only one field and one constructor
