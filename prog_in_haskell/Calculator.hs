module Calculator (
    main
) where

import Interactive

main :: IO ()
main = return ()

box :: [String]
box = [
        "+---------------+"
      , "|               |"
      , "+---+---+---+---+"
      , "| q | c | d | = |"
      , "+---+---+---+---+"
      , "| 1 | 2 | 3 | + |"
      , "+---+---+---+---+"
      , "| 4 | 5 | 6 | - |"
      , "+---+---+---+---+"
      , "| 7 | 8 | 9 | * |"
      , "+---+---+---+---+"
      , "| 0 | ( | ) | / |"
      , "+---+---+---+---+"
      ]

buttons :: [Char]
buttons = standard ++ extra
  where
    standard = "qcd=123+456-789*0()/"
    extra    = "QCD \ESC\BS\DEL\n"

showbox :: IO ()
showbox = seqn [writeat (1, y) xs | (y, xs) <- zip [1..] box]

display
  :: String
  -> IO ()
display xs = do
               writeat (3, 2) "                 "
               writeat (3, 2) (reverse (take 13 (reverse xs)))

calc
  :: String
  -> IO ()
calc xs = do
            display xs
            c <- getChar
            if c `elem` buttons
              then process c xs
              else do
                     beep
                     calc xs

process
  :: Char
  -> String
  -> IO ()
process c xs
  | c `elem` "qQ\ESC"    = quit
  | c `elem` "dD\BS\DEL" = delete xs
  | c `elem` "=\n"       = eval xs
  | c `elem` "cC"        = clear
  | otherwise            = press c xs

quit :: IO ()
quit = goto (1, 14)

delete
  :: String
  -> IO ()
delete "" = calc ""
delete xs = calc (init xs)

eval
  :: String
  -> IO ()
eval xs = return () -- TODO cannot continue because "parse" does not work

clear :: IO ()
clear = calc ""

press
  :: Char
  -> String
  -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do
        cls
        showbox
        clear
