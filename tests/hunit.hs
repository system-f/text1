module Main where

import Prelude
import Control.Lens
import Control.Monad (when)
import Data.Char
import qualified Data.Text as Text
import Data.Text1

import System.Exit (exitFailure)
import Test.HUnit

main :: IO ()
main = do
  c <- runTestTT tests
  when (errors c > 0 || failures c > 0) exitFailure

tests :: Test
tests = TestList [length1Tests, compareLength1Tests, head1Tests, tail1Tests, last1Tests, init1Tests]

labelled :: String -> [Assertion] -> Test
labelled s = TestLabel s . TestList . fmap TestCase

length1Tests :: Test
length1Tests = labelled "length1" [
    assertEqual "1" (fmap length1 ("a" ^? _Text1)) (Just 1)
  , assertEqual "2" (fmap length1 ("abc" ^? _Text1)) (Just 3)
  ]

compareLength1Tests :: Test
compareLength1Tests = labelled "compareLength1" [
    assertEqual "1" (fmap (`compareLength1` 1) ("a" ^? _Text1)) (Just EQ)
  , assertEqual "2" (fmap (`compareLength1` 3) ("a" ^? _Text1)) (Just LT)
  , assertEqual "3" (fmap (`compareLength1` 1) ("abc" ^? _Text1)) (Just GT)
  , assertEqual "4" (fmap (`compareLength1` 3) ("abc" ^? _Text1)) (Just EQ)
  , assertEqual "5" (fmap (`compareLength1` 5) ("abc" ^? _Text1)) (Just LT)
  ]

head1Tests :: Test
head1Tests = labelled "head1" [
    assertEqual "1" (fmap (^. _head1) ("a" ^? _Text1)) (Just 'a')
  , assertEqual "2" (fmap (^. _head1) ("abc" ^? _Text1)) (Just 'a')
  , assertEqual "3" (fmap (show . (_head1 %~ toUpper)) ("abc" ^? _Text1)) (Just "\"Abc\"")
  ]

tail1Tests :: Test
tail1Tests = labelled "tail1" [
    assertEqual "1" (fmap (show . (^. _tail1)) ("a" ^? _Text1)) (Just "\"\"")
  , assertEqual "2" (fmap (show . (^. _tail1)) ("abc" ^? _Text1)) (Just "\"bc\"")
  , assertEqual "3" (fmap (show . (_tail1 %~ Text.toUpper)) ("abc" ^? _Text1)) (Just "\"aBC\"")
  ]

last1Tests :: Test
last1Tests = labelled "last1" [
    assertEqual "1" (fmap (^. _last1) ("a" ^? _Text1)) (Just 'a')
  , assertEqual "2" (fmap (^. _last1) ("abc" ^? _Text1)) (Just 'c')
  , assertEqual "3"  (fmap (show . (_last1 %~ toUpper)) ("abc" ^? _Text1)) (Just "\"abC\"")
  ]

init1Tests :: Test
init1Tests = labelled "init1" [
    assertEqual "1" (fmap (show . view _init1) ("a" ^? _Text1)) (Just "\"\"")
  , assertEqual "2" (fmap (show . view _init1) ("abc" ^? _Text1)) (Just "\"ab\"")
  , assertEqual "3" (fmap (show . (_init1 %~ Text.toUpper)) ("a" ^? _Text1)) (Just "\"a\"")
  , assertEqual "4" (fmap (show . (_init1 %~ Text.toUpper)) ("abc" ^? _Text1)) (Just "\"ABc\"")
  ]
