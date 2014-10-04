{-# LANGUAGE ScopedTypeVariables, NoMonomorphismRestriction #-}
import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.Time (ClockTime(..)) -- getModificationTime does not return a ClockTime. That's why we import UTCTime and changed our Predicate accordingly
import Data.Time.Clock (UTCTime)
import System.FilePath (takeExtension)
import Control.Exception (bracket, handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile)

import RecursiveContents (getRecursiveContents)

type Predicate = FilePath        -- the path
               -> Permissions    -- the permissions
               -> Maybe Integer  -- the file size
               -> UTCTime      -- last modified
               -> Bool           -- whether it matches
-- our type Predicate is essentially a synonym for a function that takes 4 arguments

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle (\(e::SomeException) -> return Nothing) $
  bracket (openFile path ReadMode) hClose $ \h -> do
    size <- hFileSize h
    return (Just size)
-- let's look at this closely
-- "handle" takes a function, an IO action and returns an IO action
-- if exception, it performs the function and return the IO action within, else the 2nd argument. In this case, "bracket" is its second argument
-- "bracket" takes 3 arguments
-- 1st acquires a resource
-- 2nd release the same resource
-- 3rd is the actual "use" action
-- if 1st succeeds, 2nd will ALWAYS be called
-- if 3rd fails, 2nd will be called, and the exception is thrown upwards
-- if 3rd success, 2nd will be called and bracket returns the output from 3rd
-- i.e. if anything fails, the file handle will ALWAYS be hClose-d
betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  -- in order to keep our predicate pure,
  -- we have "check" do all the IO tasks
  where check name = do
            perms <- getPermissions name
            size <- getFileSize name
            modified <- getModificationTime name
            return (p name perms size modified)

-- this method is not safe because it does not handle exceptions caused by file's read permission, or even whether the file exists
simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size

-- even though this is safer,
-- when an exception is encountered the line "hClose h"
-- will NOT be executed
-- so this handle h is still in memory
-- and because we encountered an exception, the garbage collection won't work since the data is no longer reachable from within this program
-- thus we need to use "bracket"
saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle (\(e::SomeException) -> return Nothing) $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return $ Just size

-- A simple function to check for ".cpp" files larger than 128KB
-- It always ignores 2 arguments, and needs 2 equations
-- Not very good
myTest path _ (Just size) _ =
  takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False

-- Let's work on a solution
pathP path _ _ _ = path
-- Right now pathP has no type signature, so Haskell compiler will infer a very general type
-- To prevent error messages that are hard to understand, we give it a type
type InfoP a =  FilePath
             -> Permissions
             -> Maybe Integer
             -> UTCTime
             -> a
-- Look at how we create this type synonym:
-- InfoP a = ........ -> a
-- So instead of having multiple type synonyms, that behaves similarly, like:
-- PathP = FilePath ................... -> FilePath
-- PermissionP => FilePath ............ -> Permissions
-- We can just specify using the "a" variable
pathP :: InfoP FilePath

-- Similarly for the others
sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

permissionsP :: InfoP Permissions
permissionsP _ permissions _ _ = permissions

-- equalP is a instance of the Eq typeclass
-- It takes a InfoP a type, then an "a", and returns a InfoP Bool
-- It is worth noticing that "Predicate" is essentially the same as "InfoP Bool"
-- Now what is "f" in equalP? 
-- "f" is sizeP and pathP and etc...
-- "k" is the value to match
-- So imagine:
-- equalP sizeP 1024 = \w x y z -> sizeP w x y z == 1024
-- i.e. equalP returns a function, waiting for the four arguments:
-- FilePath, Permissions, Maybe Integer and UTCTime
-- i.e. our predicate for betterFind!
equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

-- What have we done? We created equalP, a simpler way to create our predicates to be used with betterFind, instead of using predicates like myTest

-- Now, other than "equal", we need greater and lesser too
-- However again, it is not very good to write a complete definition for them.
-- This is referred to "Avoiding Boilerplate with Lifting"
-- Instead, we write an abstract one
liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` k
-- Now what is "q"?
-- "q" is our binary operator, (==), (<), (>) etc.
-- so now equalP can now be defined using liftP
equalP'' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP'' = liftP (==)

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

-- So far we can only check with one part of the predicate
-- to check multiple properties, we need to glue the predicates together
simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g w x y z = f w x y z && g w x y z
-- But wait, now that we have liftP, we can make it better
liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z
-- So now simpleAndP can be:
simpleAndP' = liftP2 (&&)

andP = liftP2 (&&)
orP = liftP2 (||)

-- In fact, liftP can be written in terms of liftP2
constP :: a -> InfoP a
constP k _ _ _ _ = k
liftP' q f k w x y z = f w x y z `q` constP k w x y z

-- Congrats, because by now you have written a few "combinators"
-- Combinators are functions that take other functions as arguments
-- With our combinators...
liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

myTest2 = (liftPath takeExtension `equalP` ".cpp") `andP`
          (sizeP `greaterP` 131072)

--(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
-- We either add the type signature, or add:
-- Probable fix: give these definition(s) an explicit type signature
--                   or use -XNoMonomorphismRestriction
(==?) = equalP
(&&?) = andP
(||?) = orP
(>?) = greaterP
(<?) = lesserP
-- these definitions have to be in brackets because Haskell compiler don't know the precedence or associativity

myTest3 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)
-- :info ==, :info &&... to find out the precedence of the original operators
infix 4 ==?
infixr 3 &&?
infix 4 >?

myTest4 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)
