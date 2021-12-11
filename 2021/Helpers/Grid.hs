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
    pointsWhere,
    inBounds,
    neighboringPoints,
    neighboringPointsWithDiagonals,
    neighboringValues,
  )
where

import Data.Array hiding ((!), (//))
import qualified Data.Array as Array
import qualified Data.List as List
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
   in Grid (listArray (Point 0 0, Point (h - 1) (w - 1)) (concat rows))

fromDigits :: String -> Grid Int
fromDigits = fromList . map (map (read . pure)) . lines

width :: Grid a -> Int
width = succ . pY . snd . bounds . unGrid

height :: Grid a -> Int
height = succ . pX . snd . bounds . unGrid

lookup :: [Point] -> Grid a -> [a]
lookup point (Grid grid) = map (grid Array.!) point

(!) :: Grid a -> Point -> a
(!) grid point = head $ lookup [point] grid

allValues :: Grid a -> [a]
allValues grid = lookup (allPoints grid) grid

all :: (a -> Bool) -> Grid a -> Bool
all predicate = List.all predicate . allValues

(//) :: Grid a -> [(Point, a)] -> Grid a
(//) (Grid grid) replacements = Grid (grid Array.// replacements)

updateWith :: (a -> a -> a) -> [(Point, a)] -> Grid a -> Grid a
updateWith f updates grid = grid // map (\(c, x) -> (c, f (grid ! c) x)) updates

allPoints :: Grid a -> [Point]
allPoints = range . bounds . unGrid

pointsWhere :: (a -> Bool) -> Grid a -> [Point]
pointsWhere predicate grid = filter (predicate . (grid !)) (allPoints grid)

inBounds :: Point -> Grid a -> Bool
inBounds point (Grid grid) = inRange (bounds grid) point

neighboringPoints :: Point -> Grid a -> [Point]
neighboringPoints point grid =
  filter (`inBounds` grid) $ Point.neighboringPoints point

neighboringPointsWithDiagonals :: Point -> Grid a -> [Point]
neighboringPointsWithDiagonals point grid =
  filter (`inBounds` grid) $ Point.neighboringPointsWithDiagonals point

neighboringValues :: Point -> Grid a -> [a]
neighboringValues point grid =
  map (grid !) (neighboringPoints point grid)
