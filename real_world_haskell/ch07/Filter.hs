main :: IO ()
main = do
        interact (unlines . filter (elem 'a') . lines)
