{-# OPTIONS -Wall #-}

import qualified Data.Heap as Heap
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers.Grid (Grid, (//))
import qualified Helpers.Grid as Grid
import Helpers.Point (Point (..))

type Queue = Heap.MinHeap QueueEntry

data QueueEntry = QueueEntry Int Point
  deriving (Eq)

instance Ord QueueEntry where
  compare (QueueEntry a _) (QueueEntry b _) = compare a b

main :: IO ()
main = do
  riskLevels <- (// [(Point 0 0, 0)]) . grow . Grid.fromDigits <$> getContents
  let distance = distanceFromStart riskLevels Set.empty $ Heap.fromDescList [QueueEntry 0 (snd (Grid.bounds riskLevels))]
  print distance

grow :: Grid Int -> Grid Int
grow grid = foldr (flip (//)) Grid.empty [Grid.toList (shiftGrid y x) | y <- [0 .. 4], x <- [0 .. 4]]
  where
    (height, width) = let (_, Point maxY maxX) = Grid.bounds grid in (maxY + 1, maxX + 1)
    shiftGrid :: Int -> Int -> Grid Int
    shiftGrid yOffset xOffset = wrap . (\n -> n + yOffset + xOffset) <$> Grid.mapPoints (\(Point y x) -> Point (height * yOffset + y) (width * xOffset + x)) grid
    wrap :: Int -> Int
    wrap n = (n - 1) `mod` 9 + 1

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
