{-# OPTIONS -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}

module Helpers.Grid
  ( Grid,
    Point,
    fromList,
    fromPoints,
    fromDigits,
    width,
    height,
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
import Data.Foldable (toList)
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

data Grid a = DenseGrid (Array Point a) | SparseGrid Int Int a (Map Point a)

instance Show a => Show (Grid a) where
  show grid =
    List.intercalate "\n" $ map (\y -> unwords $ map (\x -> show (grid ! Point y x)) [0 .. (width grid - 1)]) [0 .. (height grid - 1)]

deriving instance Eq a => Eq (Grid a)

instance Functor Grid where
  fmap f (DenseGrid grid) = DenseGrid $ fmap f grid
  fmap f (SparseGrid h w defaultValue entries) = SparseGrid h w (f defaultValue) $ fmap f entries

fromList :: [[a]] -> Grid a
fromList [] = error "Empty grid."
fromList rows =
  let w = length (head rows)
      h = length rows
      bounds = (Point 0 0, Point (pred h) (pred w))
   in DenseGrid (Array.listArray bounds (concat rows))

fromPoints :: a -> Map Point a -> Grid a
fromPoints defaultValue entries =
  let pairs = Map.keys entries
      h = succ $ maximum $ map pY pairs
      w = succ $ maximum $ map pX pairs
   in SparseGrid h w defaultValue entries

fromDigits :: String -> Grid Int
fromDigits = fromList . map (map (read . pure)) . lines

width :: Grid a -> Int
width (DenseGrid grid) = succ $ pY $ snd $ Array.bounds grid
width (SparseGrid _ w _ _) = w

height :: Grid a -> Int
height (DenseGrid grid) = succ $ pX $ snd $ Array.bounds grid
height (SparseGrid h _ _ _) = h

lookup :: Set Point -> Grid a -> [a]
lookup points (DenseGrid grid) =
  map (grid Array.!) (toList points)
lookup points (SparseGrid _ _ defaultValue entries) =
  map (Maybe.fromMaybe defaultValue . (`Map.lookup` entries)) (toList points)

(!) :: Grid a -> Point -> a
(!) grid point = head $ lookup (Set.singleton point) grid

allValues :: Grid a -> [a]
allValues grid = lookup (allPoints grid) grid

all :: (a -> Bool) -> Grid a -> Bool
all predicate = List.all predicate . allValues

count :: (a -> Bool) -> Grid a -> Int
count predicate = length . filter predicate . allValues

(//) :: Grid a -> [(Point, a)] -> Grid a
(//) (DenseGrid grid) replacements = DenseGrid (grid Array.// replacements)
(//) (SparseGrid h w defaultValue grid) replacements = SparseGrid h w defaultValue (Map.fromList replacements `Map.union` grid)

updateWith :: (a -> a -> a) -> Map Point a -> Grid a -> Grid a
updateWith f updates grid = grid // map (\(c, x) -> (c, f (grid ! c) x)) (Map.toList updates)

subGrid :: Point -> Point -> Grid a -> Grid a
subGrid minP@(Point minY minX) maxP@(Point maxY maxX) grid@(DenseGrid values) =
  toList values
    |> Split.chunksOf (width grid)
    |> drop minY
    |> take (maxY - minY + 1)
    |> map (take (maxX - minX + 1) . drop minX)
    |> (DenseGrid . Array.listArray (minP, maxP) . concat)
subGrid (Point minY minX) (Point maxY maxX) (SparseGrid _ _ defaultValue entries) =
  let newH = maxY - minY + 1
      newW = maxX - minX + 1
      newEntries = Map.filterWithKey (\(Point y x) _ -> x >= minX && x <= maxX && y >= minY && y <= maxY) entries
   in SparseGrid newH newW defaultValue newEntries

mapPoints :: (Point -> Point) -> Grid a -> Grid a
mapPoints _ (DenseGrid _) = error "Cannot map points of a dense grid."
mapPoints f (SparseGrid _ _ defaultValue entries) = fromPoints defaultValue (Map.mapKeys f entries)

allPoints :: Grid a -> Set Point
allPoints = Set.fromList . allPointsList

allPointsList :: Grid a -> [Point]
allPointsList (DenseGrid grid) = Array.range $ Array.bounds grid
allPointsList (SparseGrid h w _ _) = Point.allPointsWithinBounds h w

pointsWhere :: (a -> Bool) -> Grid a -> Set Point
pointsWhere predicate grid = Set.filter (predicate . (grid !)) (allPoints grid)

inBounds :: Point -> Grid a -> Bool
inBounds point (DenseGrid grid) = Array.inRange (Array.bounds grid) point
inBounds (Point x y) (SparseGrid h w _ _) =
  x >= 0 && x < w && y >= 0 && y < h

neighboringPoints :: Point -> Grid a -> Set Point
neighboringPoints point grid =
  Set.filter (`inBounds` grid) $ Point.neighboringPoints point

neighboringPointsWithDiagonals :: Point -> Grid a -> Set Point
neighboringPointsWithDiagonals point grid =
  Set.filter (`inBounds` grid) $ Point.neighboringPointsWithDiagonals point

neighboringValues :: Point -> Grid a -> [a]
neighboringValues point grid =
  map (grid !) (toList (neighboringPoints point grid))
