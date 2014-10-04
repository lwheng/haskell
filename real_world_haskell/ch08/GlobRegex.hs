module GlobRegex
(
  globToRegex
  , matchesGlob
)
-- Recall what it means to place the functions in the brackets
-- It means we are only exposing this 2 functions
where

import Text.Regex.Posix ((=~))

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""

globToRegex' ('*':cs) = ".*" ++ globToRegex' cs

globToRegex' ('?':cs) = '.' : globToRegex' cs

globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs) = '[' : c : charClass cs
globToRegex' ('[':_) = error "Unterminated character class"

globToRegex' (c:cs) = escape c ++ globToRegex' cs

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = name =~ globToRegex pat

charClass :: String -> String
charClass (']':cs) = ']' : globToRegex' cs
charClass (c:cs) =  c : charClass cs
charClass [] = error "Unterminated character class"

escape :: Char -> String
escape c | c `elem` regexChars = '\\' : [c]
         | otherwise = [c]
  where regexChars = "\\+()^$.{}]|"
