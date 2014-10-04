-- Defining a data type that says Left, Right or Straight
data Direction = LeftTurn
                | RightTurn
                | Straight
                deriving (Show)

-- Defining a data type for a cartesian point with Double
-- Notice we are using Record Syntax
-- So that we can use this:
-- x(Point) to access its x attribute
data Point = Point { x :: Double, y :: Double}
            deriving (Show)

-- A function that says which direction the line segments are turning to
-- Had to search up a little on how to find out whether left/right turn
-- look up 'Cross Product'
-- So from the type signature, turn take 3 points and return the Direction
turn :: Point -> Point -> Point -> Direction
turn p1 p2 p3
  | prod > 0 = LeftTurn
  | prod < 0 = RightTurn
  | otherwise = Straight
  where prod = ((x(p2) - x(p1)) * (y(p3) - y(p1))) -
               ((y(p2) - y(p1)) * (x(p3) - x(p1)))

point1 = Point 1 2
point2 = Point 2 5
point3 = Point 3 10
turns :: [Point] -> [Direction]
-- Since we need 3 points, we add the 2 base cases too
turns [] = []
turns [a,b] = []
turns points = processThreePoints (take 3 points) : turns (tail points)
  where processThreePoints [a,b,c] = turn a b c
-- we wrote a helper function processThreePoints that help us call the turn function
-- then we cons and recursively call turns on the tail of points
