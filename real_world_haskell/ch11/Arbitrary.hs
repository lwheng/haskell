module
Arbitrary
where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Arbitrary
import System.Random

{-
class Arbitrary a where
  arbitrary :: Gen a
  elements :: [a] -> Gen a -- Take a list of values and returns a generator
  choose :: Random a => (a, a) -> Gen a -- Take a value from the generator
  oneof :: [Gen a] -> Gen a -- Take one of the generators
-}

-- E.g.
data Ternary = Yes
             | No
             | Unknown
             deriving (Show, Eq)

instance Arbitrary Ternary where
  arbitrary = elements [Yes, No, Unknown]

-- Create a generator using primitive types, and then return accordingly
instance Arbitrary Ternary where
  arbitrary = do
    n <- choose (0,2) :: Gen Int
    return $ case n of
                  0 -> Yes
                  1 -> No
                  2 -> Unknown

-- For structures and tuples, we need to generate each component separately
instance Arbitrary Ternary where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return (x,y)
