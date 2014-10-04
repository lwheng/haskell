import Control.Monad (ap, liftM, liftM3)

data MovieReview = MovieReview {
    revTitle :: String
  , revUser :: String
  , revReview :: String
}

-- First naive approach
simpleReview :: [(String, Maybe String)] -> Maybe MovieReview
simpleReview alist =
  case lookup "title" alist of
    Just (Just title@(_:_)) ->
      case lookup "user" alist of
        Just (Just user@(_:_)) ->
          case lookup "review" alist of
            Just (Just review@(_:_)) ->
              Just (MovieReview title user review)
            _ -> Nothing -- no review
        _ -> Nothing -- no user
    _ -> Nothing -- no title

-- first attempt
maybeReview alist = do
    title <- lookup1 "title" alist
    user <- lookup1 "user" alist
    review <- lookup1 "review" alist
    return (MovieReview title user review)

lookup1 key alist = case lookup key alist of
                      Just (Just s@(_:_)) -> Just s
                      _ -> Nothing

-- second attempt
liftedReview alist =
    liftM3 MovieReview (lookup1 "title" alist)
                       (lookup1 "user" alist)
                       (lookup1 "review" alist)
-- Although liftM3 tidies up our code, in general, we cannot extend to whatever number we please as standard libraries define up to liftM5 only
-- We need another method

-- Look at the type signature of "ap" under Control.Monad
-- ap :: Monad m => m (a -> b) -> m a -> m b
-- Recall all haskell functions are really single argument functions
-- So MovieReview is actually: String -> (String -> (String -> MovieReview))

apReview alist = 
    MovieReview `liftM` lookup1 "title" alist
                   `ap` lookup1 "user" alist
                   `ap` lookup1 "review" alist
