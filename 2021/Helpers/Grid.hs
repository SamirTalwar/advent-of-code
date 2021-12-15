{-# OPTIONS -Wall #-}

module Helpers.Grid
  ( Grid,
    Point,
    empty,
    fromList,
    fromPoints,
    fromDigits,
    toList,
    bounds,
    lookup,
    (!),
    all,
    count,
    (//),
    updateWith,
    subGrid,
    allPoints,
    allPointsList,
    pointsWhere,
    mapPoints,
    inBounds,
    neighboringPoints,
    neighboringPointsWithDiagonals,
    neighboringValues,
  )
where

import Data.Array (Array)
import qualified Data.Array as Array
import Data.Function
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers.Function
import Helpers.Point (Point (..))
import qualified Helpers.Point as Point
import Prelude hiding (all, lookup)

data Grid a = DenseGrid (Array Point a) | SparseGrid (Point, Point) a (Map Point a)

instance Show a => Show (Grid a) where
  show grid =
    let (Point startY startX, Point endY endX) = bounds grid
     in List.intercalate "\n" $ map (\y -> unwords $ map (\x -> show (grid ! Point y x)) [startX .. endX]) [startY .. endY]

instance Eq a => Eq (Grid a) where
  DenseGrid valuesA == DenseGrid valuesB = valuesA == valuesB
  SparseGrid _ _ entriesA == SparseGrid _ _ entriesB =
    entriesA == entriesB
  a == b = toSparse a == toSparse b

instance Ord a => Ord (Grid a) where
  compare = compare `on` allValues

instance Functor Grid where
  fmap f (DenseGrid values) = DenseGrid $ fmap f values
  fmap f (SparseGrid gridBounds defaultValue entries) = SparseGrid gridBounds (f defaultValue) $ fmap f entries

instance Foldable Grid where
  foldMap f (DenseGrid values) = foldMap f values
  foldMap f (SparseGrid _ _ entries) = foldMap f entries

empty :: Grid a
empty = fromPoints undefined Map.empty

fromList :: [[a]] -> Grid a
fromList [] = empty
fromList rows =
  let w = length (head rows)
      h = length rows
      gridBounds = (Point 0 0, Point (pred h) (pred w))
   in DenseGrid (Array.listArray gridBounds (concat rows))

fromPoints :: a -> Map Point a -> Grid a
fromPoints defaultValue entries = SparseGrid gridBounds defaultValue entries
  where
    points = Map.keysSet entries
    xs = Set.map pX points
    ys = Set.map pY points
    gridBounds =
      if null points
        then (Point 0 0, Point 0 0)
        else (Point (Set.findMin ys) (Set.findMin xs), Point (Set.findMax ys) (Set.findMax xs))

fromDigits :: String -> Grid Int
fromDigits = fromList . map (map (read . pure)) . lines

toList :: Grid a -> [(Point, a)]
toList grid@(DenseGrid values) = allPointsList grid `zip` Array.elems values
toList (SparseGrid _ _ entries) = Map.toList entries

bounds :: Grid a -> (Point, Point)
bounds (DenseGrid values) = Array.bounds values
bounds (SparseGrid gridBounds _ _) = gridBounds

lookup :: Set Point -> Grid a -> [a]
lookup points (DenseGrid values) =
  map (values Array.!) (Set.toList points)
lookup points (SparseGrid _ defaultValue entries) =
  map (Maybe.fromMaybe defaultValue . (`Map.lookup` entries)) (Set.toList points)

(!) :: Grid a -> Point -> a
(!) grid point = head $ lookup (Set.singleton point) grid

allValues :: Grid a -> [a]
allValues grid = lookup (allPoints grid) grid

all :: (a -> Bool) -> Grid a -> Bool
all predicate = List.all predicate . allValues

count :: (a -> Bool) -> Grid a -> Int
count predicate = length . filter predicate . allValues

(//) :: Grid a -> [(Point, a)] -> Grid a
(//) (DenseGrid values) replacements = DenseGrid (values Array.// replacements)
(//) (SparseGrid _ defaultValue entries) replacements = fromPoints defaultValue (Map.fromList replacements `Map.union` entries)

updateWith :: (a -> a -> a) -> Map Point a -> Grid a -> Grid a
updateWith f updates grid = grid // map (\(c, x) -> (c, f (grid ! c) x)) (Map.toList updates)

subGrid :: Point -> Point -> Grid a -> Grid a
subGrid start@(Point startY startX) end@(Point endY endX) grid@(DenseGrid values) =
  Array.elems values
    |> Split.chunksOf (let (Point _ minX, Point _ maxX) = bounds grid in maxX - minX + 1)
    |> drop startY
    |> take (endY - startY + 1)
    |> map (take (endX - startX + 1) . drop startX)
    |> (DenseGrid . Array.listArray (start, end) . concat)
subGrid (Point startY startX) (Point endY endX) (SparseGrid _ defaultValue entries) =
  let newEntries = Map.filterWithKey (\(Point y x) _ -> x >= startX && x <= endX && y >= startY && y <= endY) entries
   in fromPoints defaultValue newEntries

mapPoints :: (Point -> Point) -> Grid a -> Grid a
mapPoints f grid@DenseGrid {} = mapPoints f $ toSparse grid
mapPoints f (SparseGrid _ defaultValue entries) = fromPoints defaultValue (Map.mapKeys f entries)

allPoints :: Grid a -> Set Point
allPoints = Set.fromList . allPointsList

allPointsList :: Grid a -> [Point]
allPointsList grid = Array.range $ bounds grid

pointsWhere :: (a -> Bool) -> Grid a -> Set Point
pointsWhere predicate grid = Set.filter (predicate . (grid !)) (allPoints grid)

inBounds :: Point -> Grid a -> Bool
inBounds point grid = Array.inRange (bounds grid) point

neighboringPoints :: Point -> Grid a -> Set Point
neighboringPoints point grid =
  Set.filter (`inBounds` grid) $ Point.neighboringPoints point

neighboringPointsWithDiagonals :: Point -> Grid a -> Set Point
neighboringPointsWithDiagonals point grid =
  Set.filter (`inBounds` grid) $ Point.neighboringPointsWithDiagonals point

neighboringValues :: Point -> Grid a -> [a]
neighboringValues point grid =
  map (grid !) (Set.toList (neighboringPoints point grid))

toSparse :: Grid a -> Grid a
toSparse grid@(DenseGrid values) = fromPoints undefined $ Map.fromList $ zip (allPointsList grid) (Array.elems values)
toSparse grid@SparseGrid {} = grid
