{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections #-}

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Helpers.Grid (Grid, (//))
import qualified Helpers.Grid as Grid
import Helpers.Point (Point (..))

main :: IO ()
main = do
  riskLevels <- (// [(Point 0 0, 0)]) . grow . Grid.fromDigits <$> getContents
  let cumulativeRiskLevels = cumulative riskLevels Map.empty [(snd (Grid.bounds riskLevels), 0)]
  print $ cumulativeRiskLevels ! Point 0 0

grow :: Grid Int -> Grid Int
grow grid = foldr (flip (//)) Grid.empty [Grid.toList (shiftGrid y x) | y <- [0 .. 4], x <- [0 .. 4]]
  where
    (height, width) = let (_, Point maxY maxX) = Grid.bounds grid in (maxY + 1, maxX + 1)
    shiftGrid :: Int -> Int -> Grid Int
    shiftGrid yOffset xOffset = wrap . (\n -> n + yOffset + xOffset) <$> Grid.mapPoints (\(Point y x) -> Point (height * yOffset + y) (width * xOffset + x)) grid
    wrap :: Int -> Int
    wrap n = (n - 1) `mod` 9 + 1

cumulative :: Grid Int -> Map Point Int -> [(Point, Int)] -> Map Point Int
cumulative _ cumulativeValues [] = cumulativeValues
cumulative grid cumulativeValues ((point@(Point 0 0), cumulativeValue) : _) =
  Map.insert point (cumulativeValue + (grid Grid.! point)) cumulativeValues
cumulative grid cumulativeValues ((point, cumulativeValue) : rest)
  | point `Map.member` cumulativeValues =
    cumulative grid cumulativeValues rest
  | otherwise =
    let newValue = cumulativeValue + (grid Grid.! point)
        newCumulativeValues = Map.insert point newValue cumulativeValues
        neighbors = map (,newValue) $ Set.toList $ Grid.neighboringPoints point grid
        next = mergeSortedOn snd neighbors rest
     in cumulative grid newCumulativeValues next

mergeSortedOn :: Ord b => (a -> b) -> [a] -> [a] -> [a]
mergeSortedOn _ xs [] = xs
mergeSortedOn _ [] ys = ys
mergeSortedOn f (x : xs) (y : ys)
  | f x <= f y = x : mergeSortedOn f xs (y : ys)
  | otherwise = y : mergeSortedOn f (x : xs) ys
