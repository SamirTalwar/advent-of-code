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

newtype Grid a = Grid {unGrid :: Array Point a}

instance Show a => Show (Grid a) where
  show grid =
    List.intercalate "\n" $ map (\y -> unwords $ map (\x -> show (grid ! Point y x)) [0 .. (width grid - 1)]) [0 .. (height grid - 1)]

deriving instance Eq a => Eq (Grid a)

instance Functor Grid where
  fmap f = Grid . fmap f . unGrid

fromList :: [[a]] -> Grid a
fromList [] = error "Empty grid."
fromList rows =
  let w = length (head rows)
      h = length rows
   in Grid (Array.listArray (Point 0 0, Point (h - 1) (w - 1)) (concat rows))

fromDigits :: String -> Grid Int
fromDigits = fromList . map (map (read . pure)) . lines

width :: Grid a -> Int
width = succ . pY . snd . Array.bounds . unGrid

height :: Grid a -> Int
height = succ . pX . snd . Array.bounds . unGrid

lookup :: Set Point -> Grid a -> [a]
lookup point (Grid grid) = map (grid Array.!) (Set.toList point)

(!) :: Grid a -> Point -> a
(!) grid point = head $ lookup (Set.singleton point) grid

allValues :: Grid a -> [a]
allValues grid = lookup (allPoints grid) grid

all :: (a -> Bool) -> Grid a -> Bool
all predicate = List.all predicate . allValues

(//) :: Grid a -> [(Point, a)] -> Grid a
(//) (Grid grid) replacements = Grid (grid Array.// replacements)

updateWith :: (a -> a -> a) -> Map Point a -> Grid a -> Grid a
updateWith f updates grid = grid // map (\(c, x) -> (c, f (grid ! c) x)) (Map.toList updates)

allPoints :: Grid a -> Set Point
allPoints = Set.fromList . allPointsList

allPointsList :: Grid a -> [Point]
allPointsList = Array.range . Array.bounds . unGrid

pointsWhere :: (a -> Bool) -> Grid a -> Set Point
pointsWhere predicate grid = Set.filter (predicate . (grid !)) (allPoints grid)

inBounds :: Point -> Grid a -> Bool
inBounds point (Grid grid) = Array.inRange (Array.bounds grid) point

neighboringPoints :: Point -> Grid a -> Set Point
neighboringPoints point grid =
  Set.filter (`inBounds` grid) $ Point.neighboringPoints point

neighboringPointsWithDiagonals :: Point -> Grid a -> Set Point
neighboringPointsWithDiagonals point grid =
  Set.filter (`inBounds` grid) $ Point.neighboringPointsWithDiagonals point

neighboringValues :: Point -> Grid a -> [a]
neighboringValues point grid =
  map (grid !) (Set.toList (neighboringPoints point grid))
