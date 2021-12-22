{-# OPTIONS -Wall #-}

import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers.Grid (Grid)
import qualified Helpers.Grid as Grid
import Helpers.Point (Point (..))

type Queue = Set QueueEntry

data QueueEntry = QueueEntry Int Point
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  riskLevels <- Grid.update (Map.singleton (Point 0 0) 0) . grow . map (map (read . pure)) . lines <$> getContents
  let distance = distanceFromStart riskLevels Set.empty $ Set.singleton $ QueueEntry 0 (snd (Grid.bounds riskLevels))
  print distance

grow :: [[Int]] -> Grid Int
grow grid = Grid.fromList wrapped
  where
    repeatedX = map (concat . zipWith (\i chunk -> map (+ i) chunk) [0 ..] . replicate 5) grid
    repeated = concat $ zipWith (\i chunk -> map (map (+ i)) chunk) [0 ..] $ replicate 5 repeatedX
    wrapped = map (map wrap) repeated
    wrap n = (n - 1) `mod` 9 + 1

distanceFromStart :: Grid Int -> Set Point -> Queue -> Int
distanceFromStart grid done queue =
  case Set.deleteFindMin queue of
    (QueueEntry cumulativeValue point@(Point 0 0), _) ->
      cumulativeValue + (grid Grid.! point)
    (QueueEntry _ point, rest)
      | point `Set.member` done ->
        distanceFromStart grid done rest
    (QueueEntry cumulativeValue point, rest) ->
      let newValue = cumulativeValue + (grid Grid.! point)
          newDone = Set.insert point done
          neighbors = Set.map (QueueEntry newValue) $ Grid.neighboringPoints point grid
          next = Set.union neighbors rest
       in distanceFromStart grid newDone next
