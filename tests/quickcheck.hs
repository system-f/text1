module Main where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Semigroup ((<>))
import Test.QuickCheck
import Data.Text (Text, pack)
import Data.Text1
import Data.Foldable (traverse_)
import Data.Traversable (traverse)
import System.Exit (exitFailure)

main :: IO ()
main =
  traverse quickCheckResult tests >>= traverse_ dieOnFail
  where
    dieOnFail x = case x of
      Success {} -> pure ()
      _          -> exitFailure

tests :: [Property]
tests = [property semigroupAssoc, property length1GT1, property compareLength1NotLT]

newtype T  = T  { unT  :: Text  } deriving (Eq, Ord, Show)
newtype T1 = T1 { unT1 :: Text1 } deriving (Eq, Ord, Show)

instance Arbitrary T  where
  arbitrary = fmap (T . pack) arbitrary

instance Arbitrary T1 where
  arbitrary = T1 <$> (Text1 <$> arbitrary <*> (unT <$> arbitrary))

semigroupAssoc :: T1 -> T1 -> T1 -> Property
semigroupAssoc (T1 x) (T1 y) (T1 z) = (x <> (y <> z)) === ((x <> y) <> z)

length1GT1 :: T1 -> Bool
length1GT1 (T1 t) = length1 t >= 1

compareLength1NotLT :: T1 -> Bool
compareLength1NotLT (T1 t) = compareLength1 t 1 /= LT
