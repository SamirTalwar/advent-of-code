{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections #-}

import Data.Foldable (toList)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Helpers.Grid (Grid, (!), (//))
import qualified Helpers.Grid as Grid
import Helpers.Point (Point (..))

main :: IO ()
main = do
  riskLevels <- (// [(Point 0 0, 0)]) . Grid.fromDigits <$> getContents
  let cumulativeRiskLevels = cumulative riskLevels (Map.singleton (snd (Grid.bounds riskLevels)) 0)
  print $ cumulativeRiskLevels ! Point 0 0

cumulative :: Grid Int -> Map Point Int -> Grid Int
cumulative grid current
  | null current = grid
  | otherwise =
    let updates = Map.fromListWith min $ map (\(point, restValue) -> (point, (grid ! point) + restValue)) $ Map.toList current
        updatedGrid = grid // Map.toList updates
        next = Map.fromListWith min $ concatMap (\point -> map (,updatedGrid ! point) $ filter (< point) $ toList $ Grid.neighboringPoints point grid) $ Map.keys updates
     in cumulative updatedGrid next
