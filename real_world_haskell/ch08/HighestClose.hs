import qualified Data.ByteString.Lazy.Char8 as L

-- (!!4) returns the 4th element in the list
-- (!!) :: [a] -> Int -> a
closing = readPrice . (!!4) . L.split ','

readPrice :: L.ByteString -> Maybe Int
readPrice str = 
  case L.readInt str of
    Nothing -> Nothing
    -- Pattern match dollars and cents, except that the return value is
    -- (Int, L.ByteString)
    -- That's why we need to "case" it again
    Just (dollars,rest) ->
      case L.readInt (L.tail rest) of
        Nothing -> Nothing
        -- At this point "more" should be blank/empty already
        Just (cents,more) -> Just (dollars * 100 + cents)

-- (Nothing:)
-- A trick to make sure "maximum" is not applied onto an empty list
highestClose = maximum . (Nothing:) . map closing . L.lines

highestCloseFrom path = do
  contents <- L.readFile path
  print $ highestClose contents
