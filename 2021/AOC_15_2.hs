{-# OPTIONS -Wall #-}

import qualified Data.Heap as Heap
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers.Grid (Grid)
import qualified Helpers.Grid as Grid
import Helpers.Point (Point (..))

type Queue = Heap.MinHeap QueueEntry

data QueueEntry = QueueEntry Int Point
  deriving (Eq)

instance Ord QueueEntry where
  compare (QueueEntry a _) (QueueEntry b _) = compare a b

main :: IO ()
main = do
  riskLevels <- Grid.update (Map.singleton (Point 0 0) 0) . grow . Grid.fromDigits <$> getContents
  let distance = distanceFromStart riskLevels Set.empty $ Heap.fromDescList [QueueEntry 0 (snd (Grid.bounds riskLevels))]
  print distance

grow :: Grid Int -> Grid Int
grow grid = Grid.fromPoints undefined $ Map.fromList repeatedPoints
  where
    points = Grid.toList grid
    repeatedPoints = do
      y <- [0 .. 4]
      x <- [0 .. 4]
      shiftPoint y x <$> points
    shiftPoint :: Int -> Int -> (Point, Int) -> (Point, Int)
    shiftPoint yOffset xOffset (Point y x, n) =
      (Point (height * yOffset + y) (width * xOffset + x), wrap (n + yOffset + xOffset))
    wrap n = (n - 1) `mod` 9 + 1
    height = maxY + 1
    width = maxX + 1
    (_, Point maxY maxX) = Grid.bounds grid

distanceFromStart :: Grid Int -> Set Point -> Queue -> Int
distanceFromStart grid done queue =
  case Heap.view queue of
    Nothing ->
      error "Failed."
    Just (QueueEntry cumulativeValue point@(Point 0 0), _) ->
      cumulativeValue + (grid Grid.! point)
    Just (QueueEntry _ point, rest)
      | point `Set.member` done ->
        distanceFromStart grid done rest
    Just (QueueEntry cumulativeValue point, rest) ->
      let newValue = cumulativeValue + (grid Grid.! point)
          newDone = Set.insert point done
          neighbors = Heap.fromDescList $ map (QueueEntry newValue) $ Set.toList $ Grid.neighboringPoints point grid
          next = Heap.union neighbors rest
       in distanceFromStart grid newDone next
