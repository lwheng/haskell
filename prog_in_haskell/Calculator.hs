module Calculator (
    main
) where

main :: IO ()
main = return ()

beep :: IO ()
beep = putStr "\BEL"

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto
  :: Pos
  -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeat
  :: Pos
  -> String
  -> IO ()
writeat p xs = do
                 goto p
                 putStr xs

seqn
  :: [IO a]
  -> IO ()
seqn []     = return ()
seqn (a:as) = do
                a
                seqn as
