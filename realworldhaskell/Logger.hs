module Logger (
    Logger

  , Log
  , record
  , runLogger
) where

import Control.Monad
import Text.Regex.Posix ((=~))

type GlobError = String
type Log = [String]

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger ()
record s = Logger ((), [s])

newtype Logger a = Logger { execLogger :: (a, Log) }

instance Monad Logger where
  return a = Logger (a, [])
  m >>= k = let (a, w) = execLogger m
                n = k a
                (b, x) = execLogger n
            in
              Logger (b, w ++ x)

globToRegex :: String -> Logger String
globToRegex cs =
  globToRegex' cs >>= \ds ->
  return ('^':ds)

globToRegex' :: String -> Logger String
globToRegex' "" = return "$"
globToRegex' ('*':cs) = do
  record "kleene star"
  ds <- globToRegex' cs
  return (".*" ++ ds)
globToRegex' ('?':cs) =
  record "any" >>
  globToRegex' cs >>= \ds ->
  return ('.':ds)
globToRegex' ('[':'!':c:cs) =
  record "character class, negative" >>
  charClass cs >>= \ds ->
  return ("[^" ++ c : ds)
globToRegex' ('[':c:cs) =
  record "character class" >>
  charClass cs >>= \ds ->
  return ('[' : c : ds)
globToRegex' ('[':_) =
  fail "unterminated character class"
globToRegex' (c:cs) = liftM2 (++) (escape c) (globToRegex' cs)

escape :: Char -> Logger String
escape c
  | c `elem` regexChars =
      record "escape" >>
      return ['\\',c]
  | otherwise = return [c]
    where regexChars = "\\+()^$.{}]|"

charClass :: String -> Logger String
charClass (']':cs) = (']':) `liftM` globToRegex' cs
charClass (c:cs) = (c:) `liftM` charClass cs
charClass [] = fail "unterminated character class"

--matchesGlob :: FilePath -> String -> Bool
--name `matchesGlob` pattern = name =~ (globToRegex pattern) 
