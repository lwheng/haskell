-- Introducing local variable
-- Intro local variable by using the LET..IN construct
-- so LET these variable be available IN the following scope

lend amount balance = let reserve = 100
                          newBalance = balance - amount
                      in
                          if balance < reserve
                          then Nothing
                          else Just newBalance

lend2 amount balance = if amount < reserve * 0.5
                       then Just newBalance
                       else Nothing
                    where reserve = 100
                          newBalance = balance - amount
-- it is possible to intro the variable after

-- Now to see guards in action
lend3 amount balance
  | amount <= 0 = Nothing
  | amount > reserve * 0.5 = Nothing
  | otherwise = Just newBalance
  where newBalance = balance - amount
        reserve = 100
-- otherwise is for everything else
