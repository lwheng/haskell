{-
 - This file defines a Holiday
 -}

-- StartDate is a date in DDDDMMYY format
type StartDate = String
-- EndDate is a date in DDDDMMYY format
type EndDate = String
-- Summary is the holiday's name
type Summary = String

-- Here we define our Holiday type
data Holiday = Holiday {
    startDate   ::    StartDate
  , endDate     ::    EndDate
  , summary     ::    Summary
} deriving (Show)
