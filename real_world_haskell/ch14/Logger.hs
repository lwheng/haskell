module Logger
  (
    Logger
  , Log
  , runLogger
  , record
  ) where

import Control.Monad

newtype Logger a = Logger { execLogger :: (a, Log) }

instance Monad Logger where
    return a = Logger (a, [])
    m >>= k = let (a, w) = execLogger m -- we run action m
                  n      = k a          -- we apply function k onto a
                  (b, x) = execLogger n -- we run action n
              in Logger (b, w ++ x)
-- k applies a function onto a, and then wraps it with the Logger monad

-- Logger is a type constructor
globToRegex :: String -> Logger String
globToRegex cs = 
    globToRegex' cs >>= \ds ->
    return ('^':ds)

-- Takes a Pure value and wraps it with the monad's type constructor, Logger
globToRegex' :: String -> Logger String
-- Even when the function do almost nothing, we still need to wrap the results using "return"
globToRegex' "" = return "$"

-- The function does not care about the output of "record", so we use (>>) instead of (>>=)
globToRegex' ('?':cs) = 
    record "any" >>
    globToRegex' cs >>= \ds ->
    return ('.':ds)

globToRegex' ('*':cs) = 
    record "kleene star" >>
    globToRegex' cs >>= \ds ->
    return (".*" ++ ds)

globToRegex' ('[':'!':c:cs) =
    record "character class, negative" >>
    charClass cs >>= \ds ->
    return ("[^" ++ c : ds)

globToRegex' ('[':c:cs) =
    record "character class" >>
    charClass cs >>= \ds ->
    return ("[" ++ c : ds)

globToRegex' ('[':_) =
    fail "unterminated character class"

globToRegex' (c:cs) =
    liftM2 (++) (escape c) (globToRegex' cs)

-- liftM2 means a lifting function that takes 2 arguments
{-
liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 = 
    m1 >>= \a ->
    m2 >>= \b ->
    return (f a b)
-}

escape :: Char -> Logger String
escape c
    | c `elem` regexChars = record "escape" >> return ['\\',c]
    | otherwise           = return [c]
  where regexChars = "\\+()^$.{}]|"

type Log = [String]

-- Instead of giving users a value constructor
runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

-- liftM is already defined in the Control.Monad module
-- Examine the function carefully, essentially we unwrap the Monad, apply f onto its value, and then wrap the result
{-
liftM :: (Monad m) => (a -> b) -> m a -> m b
liftM f m = m >>= \i ->
            return (f i)
-}

-- Now to compare with and without usage of liftM
-- without
charClass_wordy (']':cs) = 
    globToRegex' cs >>= \ds ->
    return (']':ds)
charClass_wordy (c:cs) = 
    charClass_wordy cs >>= \ds ->
    return (c:ds)
-- with
charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs
-- to read: To apply the pure function on the left of liftM to the result of the monadic action on the right
