module NinetyNine where

import           Text.Parsec hiding (Empty)
import           Text.Parsec.String
import qualified Data.List as List

main :: IO ()
main = return ()

myLast [] = error "Expected non-empty lists"
myLast xs = (head . reverse) xs

myButLast = myLast . init

elementAt :: [a] -> Int -> a
elementAt [] _ = error "Error"
elementAt (x:xs) 1 = x
elementAt (x:xs) k = elementAt xs (k-1)

myLength :: [a] -> Int
myLength xs = myLengthHelper xs 0

myLengthHelper [] acc = acc
myLengthHelper (x:xs) acc = myLengthHelper xs (acc+1)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse xs = myReverseHelper xs []

myReverseHelper [] acc = acc
myReverseHelper (x:xs) acc = myReverseHelper xs (x : acc)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

data NestedList a = Elem a | List [NestedList a]

flatten
  :: NestedList a
  -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concat $ map flatten xs

compress :: Eq a => [a] -> [a]
compress xs = reverse $ compressHelper xs []

compressHelper :: Eq a => [a] -> [a] -> [a]
compressHelper [] acc = acc
compressHelper (x:xs) acc = case x `elem` acc of
                              True  -> compressHelper xs acc
                              False -> compressHelper xs (x:acc)

pack :: Eq a => [a] -> [[a]]
pack xs = let
            (a, b, _) = foldl f stub xs
          in
            a ++ [b]
  where
    stub :: ([[a]], [a], Maybe a)
    stub = ([], [], Nothing)
    f (accFinal, acc, curr) x = case curr of
                                  Nothing -> (accFinal, x:acc, Just x)
                                  Just c  -> case x == c of
                                               True  -> (accFinal, x:acc, Just x)
                                               False -> (accFinal ++ [acc], [x], Just x)

encode :: Eq a => [a] -> [(Int, a)]
encode xs = map (\cs -> (length cs, head cs)) $ pack xs

data Encoding a = Single a
                | Multiple Int a
                deriving (Show)

toEncoding :: (Int, a) -> Encoding a
toEncoding (i, a) = case i > 1 of
                      True -> Multiple i a
                      False -> Single a

toEncoding' :: [a] -> Encoding a
toEncoding' [] = error "Empty"
toEncoding' [x] = Single x
toEncoding' xs = Multiple (length xs) (head xs)

fromEncoding :: Encoding a -> [a]
fromEncoding (Single x) = [x]
fromEncoding (Multiple i x) = replicate i x

encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified xs = map toEncoding $ encode xs

decodeModified :: [Encoding a] -> [a]
decodeModified xs = concat $ map fromEncoding xs

encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect xs = map toEncoding' $ List.group xs

dupli :: [a] -> [a]
dupli xs = concat $ map (replicate 2) xs

repli :: [a] -> Int -> [a]
repli xs i = concat $ map (replicate i) xs

dropEvery :: [a] -> Int -> [a]
dropEvery xs k = map fst $ filter (\(x, i) -> i `mod` k /= 0) zipped
  where
    zipped = zip xs [1..]

split :: [a] -> Int -> ([a], [a])
split xs n = splitHelper n ([], xs)

splitHelper 0 (left, right) = (left, right)
splitHelper n (left, r:right) = splitHelper (n-1) (left ++ [r], right)

slice :: [a] -> Int -> Int -> [a]
slice xs l r = map fst $ filter withinRange indexed
  where
    indexed = zip xs [1..]

    withinRange (_, i) = l <= i && i <= r

rotate :: [a] -> Int -> [a]
rotate xs n
  | n == 0 = xs
  | n >  0 = drop n xs ++ take n xs
  | otherwise = drop (length xs + n) xs ++ take (length xs + n) xs

removeAt :: Int -> [a] -> [a]
removeAt n xs
  | n < 0 || n > (length xs) =  error "Index out of bounds"
  | otherwise = map fst $ filter (\(a, i) -> i /= n) $ zip xs [1..]

insertAt :: a -> [a] -> Int -> [a] 
insertAt c s i =
  let
    (left, right) = List.splitAt (i-1) s
  in
    left ++ [c] ++ right

range :: Int -> Int -> [Int]
range left right = [left..right]

-- rnd_select :: [a] -> Int -> IO [a]
-- rnd_select xs n = do
--   gen <- Random.getStdGen
--   let
--     randoms = [ xs !! x | x <- randomRs (0, (length xs) - 1) gen]
--   return $ take n randoms 

data Tree a = Empty | Branch a (Tree a) (Tree a)
              deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

empty :: Tree a
empty = Empty

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [empty]
cbalTree 1 = [leaf 'x']
cbalTree n = if n `mod` 2 == 1 then -- If n is odd, we know that the left and right branches can be the same, and the root node is the
                                    -- is the node that makes the count odd
               [ Branch 'x' l r | l <- cbalTree ((n-1) `div` 2),
                                  r <- cbalTree ((n-1) `div` 2) ]
             else
               -- It means left and right will have one node difference
               concat [ [Branch 'x' l r, Branch 'x' r l] | l <- cbalTree ((n-1) `div` 2),
                                                           r <- cbalTree (n `div` 2) ]

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ a b) (Branch _ c d) = mirror a b && mirror c d
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric t = mirror t t

add :: Ord a => a -> Tree a -> Tree a
add x Empty = leaf x
add x (Branch y l r) = case compare x y of
                         LT -> Branch y (add x l) r
                         GT -> Branch y l (add x r)
                         EQ -> Branch y l r

construct :: [Int] -> Tree Int
construct xs = foldl (flip add) Empty xs

hbaltree :: a -> Int -> [Tree a]
hbaltree x h = trees !! h
  where
    trees = [Empty] : [Branch x Empty Empty] :
            zipWith combine (tail trees) trees
    combine ts shortts = [Branch x l r | (ls, rs) <- [(shortts, ts), (ts, ts), (ts, shortts)],
                                         l <- ls,
                                         r <- rs ]

-- maximum number of nodes in a weight-balanced tree of height h
maxNodes :: Int -> Int
maxNodes h = 2^h - 1

-- minimum height of a weight-balanced tree of n nodes
minHeight :: Int -> Int
minHeight n = ceiling $ logBase 2 $ fromIntegral (n+1)

-- minimum number of nodes in a weight-balanced tree of height h
minNodes :: Int -> Int
minNodes h = fibs !! (h+2) - 1

-- maximum height of a weight-balanced tree of n nodes
maxHeight :: Int -> Int
maxHeight n = length (takeWhile (<= n+1) fibs) - 3

-- Fibonacci numbers
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

hbalTreeNodes :: a -> Int -> [Tree a]
hbalTreeNodes x n = [t | h <- [minHeight n .. maxHeight n], t <- baltree h n]
  where
        -- baltree h n = weight-balanced trees of height h with n nodes
        -- assuming minNodes h <= n <= maxNodes h
        baltree 0 n = [Empty]
        baltree 1 n = [Branch x Empty Empty]
        baltree h n = [Branch x l r |
                (hl,hr) <- [(h-2,h-1), (h-1,h-1), (h-1,h-2)],
                let min_nl = max (minNodes hl) (n - 1 - maxNodes hr),
                let max_nl = min (maxNodes hl) (n - 1 - minNodes hr),
                nl <- [min_nl .. max_nl],
                let nr = n - 1 - nl,
                l <- baltree hl nl,
                r <- baltree hr nr]

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)
tree5 = Branch 'x' (Branch 'y' Empty Empty) (Branch 'a' Empty (Branch 'b' Empty Empty))

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

leaves :: Tree a -> [a]
leaves Empty = []
leaves (Branch x Empty Empty) = [x]
leaves (Branch _ l r) = leaves l ++ leaves r

internals :: Tree a -> [a]
internals Empty = []
internals (Branch _ Empty Empty) = []
internals (Branch x l r) = x : internals l ++ internals r

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch x l r) level
  | level < 1 = error "Level needs to be at least 1"
  | level == 1 = [x]
  | otherwise = atLevelHelper (Branch x l r) 1 level

atLevelHelper :: Tree a -> Int -> Int -> [a]
atLevelHelper Empty _ _ = []
atLevelHelper (Branch x l r) currentLevel expectedLevel =
  case compare currentLevel expectedLevel of
    LT -> atLevelHelper l (currentLevel + 1) expectedLevel ++ atLevelHelper r (currentLevel + 1) expectedLevel
    EQ -> [x]
    _  -> []

completeBinaryTree :: Int -> Tree Char
completeBinaryTree n = generate_tree 1
  where
    generate_tree x
      | x > n = Empty
      | otherwise = Branch 'x' (generate_tree (2*x)) (generate_tree (2*x+1))

treeToString :: Tree Char -> String
treeToString Empty = ""
treeToString (Branch x Empty Empty) = [x]
treeToString (Branch x l r) = [x] ++ "(" ++ treeToString l ++ "," ++ treeToString r ++ ")"

pTree :: Parser (Tree Char)
pTree = do
  pBranch <|> pEmpty

pEmpty :: Parser (Tree Char)
pEmpty = return Empty

pBranch :: Parser (Tree Char)
pBranch = do
  a <- letter
  do
    char '('
    t0 <- pTree
    char ','
    t1 <- pTree
    char ')'
    return $ Branch a t0 t1
    <|> return (Branch a Empty Empty)

stringToTree str =
    case parse pTree "" str of
        Right t -> t
        Left e  -> error (show e)

-- (Root, Left, Right)
treeToPreOrder :: Tree Char -> String
treeToPreOrder Empty = ""
treeToPreOrder (Branch x l r) = x : treeToPreOrder l ++ treeToPreOrder r

-- (Left, Root, Right)
treeToInOrder :: Tree Char -> String
treeToInOrder Empty = ""
treeToInOrder (Branch x l r) = treeToInOrder l ++ [x] ++ treeToInOrder r

t = stringToTree "a(b(d,e),c(,f(g,)))"
