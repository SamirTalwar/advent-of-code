{-# OPTIONS -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}

module Helpers.Grid
  ( Grid,
    Point,
    fromList,
    fromDigits,
    width,
    height,
    lookup,
    (!),
    all,
    (//),
    updateWith,
    allPoints,
    allPointsList,
    pointsWhere,
    inBounds,
    neighboringPoints,
    neighboringPointsWithDiagonals,
    neighboringValues,
  )
where

import Data.Array (Array)
import qualified Data.Array as Array
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers.Point (Point (..))
import qualified Helpers.Point as Point
import Prelude hiding (all, lookup)

newtype Grid a = DenseGrid (Array Point a)

instance Show a => Show (Grid a) where
  show grid =
    List.intercalate "\n" $ map (\y -> unwords $ map (\x -> show (grid ! Point y x)) [0 .. (width grid - 1)]) [0 .. (height grid - 1)]

deriving instance Eq a => Eq (Grid a)

instance Functor Grid where
  fmap f (DenseGrid grid) = DenseGrid $ fmap f grid

fromList :: [[a]] -> Grid a
fromList [] = error "Empty grid."
fromList rows =
  let w = length (head rows)
      h = length rows
      bounds = (Point 0 0, Point (pred h) (pred w))
   in DenseGrid (Array.listArray bounds (concat rows))

fromDigits :: String -> Grid Int
fromDigits = fromList . map (map (read . pure)) . lines

width :: Grid a -> Int
width (DenseGrid grid) = succ $ pY $ snd $ Array.bounds grid

height :: Grid a -> Int
height (DenseGrid grid) = succ $ pX $ snd $ Array.bounds grid

lookup :: Set Point -> Grid a -> [a]
lookup point (DenseGrid grid) = map (grid Array.!) (Set.toList point)

(!) :: Grid a -> Point -> a
(!) grid point = head $ lookup (Set.singleton point) grid

allValues :: Grid a -> [a]
allValues grid = lookup (allPoints grid) grid

all :: (a -> Bool) -> Grid a -> Bool
all predicate = List.all predicate . allValues

(//) :: Grid a -> [(Point, a)] -> Grid a
(//) (DenseGrid grid) replacements = DenseGrid (grid Array.// replacements)

updateWith :: (a -> a -> a) -> Map Point a -> Grid a -> Grid a
updateWith f updates grid = grid // map (\(c, x) -> (c, f (grid ! c) x)) (Map.toList updates)

allPoints :: Grid a -> Set Point
allPoints = Set.fromList . allPointsList

allPointsList :: Grid a -> [Point]
allPointsList (DenseGrid grid) = Array.range $ Array.bounds grid

pointsWhere :: (a -> Bool) -> Grid a -> Set Point
pointsWhere predicate grid = Set.filter (predicate . (grid !)) (allPoints grid)

inBounds :: Point -> Grid a -> Bool
inBounds point (DenseGrid grid) = Array.inRange (Array.bounds grid) point

neighboringPoints :: Point -> Grid a -> Set Point
neighboringPoints point grid =
  Set.filter (`inBounds` grid) $ Point.neighboringPoints point

neighboringPointsWithDiagonals :: Point -> Grid a -> Set Point
neighboringPointsWithDiagonals point grid =
  Set.filter (`inBounds` grid) $ Point.neighboringPointsWithDiagonals point

neighboringValues :: Point -> Grid a -> [a]
neighboringValues point grid =
  map (grid !) (Set.toList (neighboringPoints point grid))
