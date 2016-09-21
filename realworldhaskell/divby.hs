data DivByError a = DivBy0
                  | ForbiddenDenominator a
  deriving (Eq, Read, Show)

divBy1
  :: Integral a
  => a
  -> [a]
  -> [a]
divBy1 numerator = map (numerator `div`)

divBy2
  :: Integral a
  => a
  -> [a]
  -> Maybe [a]
divBy2 _ []                 = Just []
divBy2 _ (0:_)              = Nothing
-- divBy2 numerator (denom:xs) = case divBy2 numerator xs of
--                                 Nothing -> Nothing
--                                 Just r  -> Just ((numerator `div` denom) : r)
divBy2 numerator denominators = mapM (numerator `safeDiv`) denominators
  where
    safeDiv _ 0 = Nothing
    safeDiv x y = Just $ x `div` y

divBy3
  :: Integral a
  => a
  -> [a]
  -> [Maybe a]
divBy3 numerator denominators = map worker denominators
  where
    worker 0 = Nothing
    worker x = Just (numerator `div` x)

divBy4
  :: Integral a
  => a
  -> [a]
  -> Maybe [a]
divBy4 = divByGeneric
-- divBy4 _ []                 = return []
-- divBy4 _ (0:_)              = fail "division by zero in divBy4"
-- divBy4 numerator (denom:xs) = divBy4 numerator xs >>= \next -> return $ (numerator `div` denom) : next

divByGeneric
  :: (Monad m, Integral a)
  => a
  -> [a]
  -> m [a]
divByGeneric _ []                 = return []
divByGeneric _ (0:_)              = fail "division by zero in divByGeneric"
divByGeneric numerator (denom:xs) = divByGeneric numerator xs >>= \next -> return $ (numerator `div` denom) : next

divBy6
  :: Integral a
  => a
  -> [a]
  -> Either String [a]
divBy6 _ []                = Right []
divBy6 _ (0:_)             = Left "divBy6: division by 0"
divBy6 numerator (denom:xs) = case divBy6 numerator xs of
                                Left x  -> Left x
                                Right r -> Right ((numerator `div` denom) : r)

divBy7
  :: Integral a
  => a
  -> [a]
  -> Either (DivByError a) [a]
divBy7 _ []                 = Right []
divBy7 _ (0:_)              = Left DivBy0
divBy7 _ (10:_)             = Left (ForbiddenDenominator 10)
divBy7 _ (20:_)             = Left (ForbiddenDenominator 20)
divBy7 numerator (denom:xs) = case divBy7 numerator xs of
                                Left x  -> Left x
                                Right r -> Right ((numerator `div` denom) : r)
