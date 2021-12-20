{-# OPTIONS -Wall #-}

module Helpers.Tests where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans (MonadIO)
import Data.Functor (($>))
import Data.IORef
import qualified Data.List as List
import qualified Data.Set as Set
import GHC.IO (unsafePerformIO)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Helpers.List
import qualified Helpers.Memoization
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

  describe "Memoization" $ do
    it "memoizes booleans" $ do
      hedgehog $ do
        b <- forAll Gen.bool
        ifFalse <- forAll $ Gen.int (Range.linear 0 100)
        ifTrue <- forAll $ Gen.int (Range.linear 0 100)
        let f False = ifFalse
            f True = ifTrue
        assertMemo f b

    it "memoizes lists" $ do
      hedgehog $ do
        xs <- forAll $ Gen.list (Range.linear 0 10) (Gen.int (Range.linear 0 maxBound))
        assertMemo sum xs

    it "memoizes sets" $ do
      hedgehog $ do
        xs <- forAll $ Set.fromList <$> Gen.list (Range.linear 0 10) (Gen.int (Range.linear 0 maxBound))
        assertMemo sum xs

assertMemo :: (MonadTest m, MonadIO m, Helpers.Memoization.HasTrie a, Eq b, Show b) => (a -> b) -> a -> m ()
assertMemo f value = do
  counter <- liftIO $ newIORef (0 :: Int)
  let inc = modifyIORef counter succ
  let memoizedF = Helpers.Memoization.memo (\x -> unsafePerformIO (inc $> f x))
  let values = iterate (const (memoizedF value)) (memoizedF value)
  take 100 values === replicate 100 (f value)
  finalCounter <- liftIO $ readIORef counter
  finalCounter === 1
