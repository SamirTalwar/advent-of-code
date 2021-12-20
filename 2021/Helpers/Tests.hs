{-# OPTIONS -Wall #-}

module Helpers.Tests where

import Control.Monad
import qualified Data.List as List
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Helpers.List
import qualified Helpers.Numbers
import Test.Hspec
import Test.Hspec.Hedgehog

main :: IO ()
main = hspec $ do
  describe "Lists" $ do
    it "selects pairs" $ do
      hedgehog $ do
        xs <- forAll $ Gen.list (Range.linear 0 10) (Gen.int (Range.linear minBound maxBound))
        forM_ (Helpers.List.selectPairs xs) (\(a, b) -> let (left, right) = List.break (== a) xs in assert (b `elem` left || b `elem` tail right))

  describe "Numbers" $ do
    it "computes triangular numbers" $ do
      hedgehog $ do
        x <- forAll $ Gen.int (Range.linear 0 100)
        Helpers.Numbers.triangular x === sum [1 .. x]

    it "computes bits" $ do
      hedgehog $ do
        x <- forAll $ Gen.int (Range.linear 0 maxBound)
        let roundTrip = Helpers.Numbers.unBits (Helpers.Numbers.bits x)
        roundTrip === x
