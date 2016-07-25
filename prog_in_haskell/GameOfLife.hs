module GameOfLife (
    main
) where

import Interactive

main :: IO ()
main = do
         cls
         showboard
         showcells glider

-- A board is a list of (x, y) positions
type Board = [Pos]

height = 5 :: Int
width  = 5 :: Int

-- | board assumes width=5 and height=5
board :: [String]
board = [
          " --- --- --- --- --- "
        , "|   |   |   |   |   |"
        , " --- --- --- --- --- "
        , "|   |   |   |   |   |"
        , " --- --- --- --- --- "
        , "|   |   |   |   |   |"
        , " --- --- --- --- --- "
        , "|   |   |   |   |   |"
        , " --- --- --- --- --- "
        , "|   |   |   |   |   |"
        , " --- --- --- --- --- "
        ]

showboard :: IO ()
showboard = do
              seqn [writeat (1, y) xs | (y, xs) <- zip [1..] board]
              putStrLn ""

glider :: Board
glider = [
           (4, 2)
         , (2, 3)
         , (4, 3)
         , (3, 4)
         , (4, 4)
         ]

calibrate
  :: Pos
  -> Pos
calibrate (x, y) = (x*4-1, y*2)

showcells :: Board -> IO ()
showcells b = do
                seqn [writeat (calibrate p) "O" | p <- b]
                goto (0,15)

isAlive
  :: Board
  -> Pos
  -> Bool
isAlive b p = p `elem` b

isEmpty
  :: Board
  -> Pos
  -> Bool
isEmpty b p = not (isAlive b p)

neighbs
  :: Pos
  -> [Pos]
neighbs (x, y) = map wrap [
                            (x-1, y-1), (x, y-1), (x+1, y-1)
                          , (x-1, y  ),           (x+1, y  )
                          , (x-1, y+1), (x, y+1), (x+1, y+1)
                          ]

wrap
  :: Pos
  -> Pos
wrap (x, y) = ((x-1 `mod` width) + 1, (y-1 `mod` height) + 1)

liveneighbs
  :: Board
  -> Pos
  -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors
  :: Board
  -> [Pos]
survivors b = [p | p <- b, (liveneighbs b p) `elem` [2,3]]

births
  :: Board
  -> [Pos]
births b = [p | p <- rmdups (concat (map neighbs b)),
                isEmpty b p,
                liveneighbs b p == 3]

rmdups
  :: Eq a
  => [a]
  -> [a]
rmdups [] = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen
  :: Board
  -> Board
nextgen b = survivors b ++ births b

wait
  :: Int
  -> IO ()
wait n = seqn [return n | _ <- [1..n]]

-- Takes a transformation function, applies it n times and returns the outcome
animate
  :: (Board -> Board)
  -> Int
  -> Board
  -> Board
animate f n b = last $ take n $ iterate f b

lifeAtStep
  :: Board
  -> Int
  -> IO ()
lifeAtStep b n = do
                   cls
                   showboard
                   showcells $ animate nextgen n b

life
  :: Board
  -> IO ()
life b = do
           cls
           showboard
           showcells b
           wait 50000
           life $ nextgen b
