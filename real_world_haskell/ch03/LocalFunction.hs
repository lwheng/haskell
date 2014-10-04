pluralise :: String -> [Int] -> [String]
pluralise word counts = map plural counts
  where plural 0 = "no " ++ word ++ "s"
        plural 1 = "one " ++ word
        plural n = show n ++ " " ++ word ++ "s"

-- defining a local function inside pluralise.
-- map will apply plural to each element of counts
-- (show n) will convert the Int n to String so that it can be of the same output type
-- (show <anything>) will attempt to convert anything to String
