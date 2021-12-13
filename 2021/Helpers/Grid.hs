{-# OPTIONS -Wall #-}

module Helpers.Grid
  ( Grid,
    Point,
    fromList,
    fromPoints,
    fromDigits,
    width,
    height,
    minX,
    maxX,
    minY,
    maxY,
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

data Grid a = DenseGrid (Array Point a) | SparseGrid Int Int a (Map Point a)

instance Show a => Show (Grid a) where
  show grid =
    List.intercalate "\n" $ map (\y -> unwords $ map (\x -> show (grid ! Point y x)) [0 .. (width grid - 1)]) [0 .. (height grid - 1)]

instance Eq a => Eq (Grid a) where
  DenseGrid valuesA == DenseGrid valuesB = valuesA == valuesB
  SparseGrid _ _ _ entriesA == SparseGrid _ _ _ entriesB =
    entriesA == entriesB
  a == b = toSparse a == toSparse b

instance Ord a => Ord (Grid a) where
  compare = compare `on` allValues

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
  if Map.null entries
    then SparseGrid 0 0 defaultValue entries
    else
      let pairs = Map.keys entries
          h = succ $ maximum $ map pY pairs
          w = succ $ maximum $ map pX pairs
       in SparseGrid h w defaultValue entries

fromDigits :: String -> Grid Int
fromDigits = fromList . map (map (read . pure)) . lines

width :: Grid a -> Int
width grid@(DenseGrid _) = maxX grid - minX grid + 1
width (SparseGrid _ w _ _) = w

height :: Grid a -> Int
height grid@(DenseGrid _) = maxX grid - minX grid + 1
height (SparseGrid h _ _ _) = h

minX :: Grid a -> Int
minX (DenseGrid grid) = pX $ fst $ Array.bounds grid
minX (SparseGrid _ _ _ entries) = minimum $ map pX $ Map.keys entries

maxX :: Grid a -> Int
maxX (DenseGrid grid) = pX $ snd $ Array.bounds grid
maxX (SparseGrid _ _ _ entries) = maximum $ map pX $ Map.keys entries

minY :: Grid a -> Int
minY (DenseGrid grid) = pY $ fst $ Array.bounds grid
minY (SparseGrid _ _ _ entries) = minimum $ map pY $ Map.keys entries

maxY :: Grid a -> Int
maxY (DenseGrid grid) = pY $ snd $ Array.bounds grid
maxY (SparseGrid _ _ _ entries) = maximum $ map pY $ Map.keys entries

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
subGrid start@(Point startY startX) end@(Point endY endX) grid@(DenseGrid values) =
  toList values
    |> Split.chunksOf (width grid)
    |> drop startY
    |> take (endY - startY + 1)
    |> map (take (endX - startX + 1) . drop startX)
    |> (DenseGrid . Array.listArray (start, end) . concat)
subGrid (Point startY startX) (Point endY endX) (SparseGrid _ _ defaultValue entries) =
  let newH = endY - startY + 1
      newW = endX - startX + 1
      newEntries = Map.filterWithKey (\(Point y x) _ -> x >= startX && x <= endX && y >= startY && y <= endY) entries
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

toSparse :: Grid a -> Grid a
toSparse grid@(DenseGrid values) = fromPoints undefined $ Map.fromList $ zip (allPointsList grid) (toList values)
toSparse grid@SparseGrid {} = grid
