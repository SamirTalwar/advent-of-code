{-# OPTIONS -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}

module Helpers.Grid
  ( Grid,
    fromList,
    fromDigits,
    width,
    height,
    lookup,
    (!),
    all,
    (//),
    updateWith,
    allCoordinates,
    coordinatesWhere,
    inBounds,
    neighboringCoordinates,
    neighboringCoordinatesWithDiagonals,
    neighboringValues,
  )
where

import Data.Array hiding ((!), (//))
import qualified Data.Array as Array
import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import qualified Data.Tuple as Tuple
import Prelude hiding (all, lookup)

newtype Grid a = Grid {unGrid :: Array (Int, Int) a}

instance Show a => Show (Grid a) where
  show grid =
    List.intercalate "\n" $ map (\y -> unwords $ map (\x -> show (grid ! (x, y))) [0 .. (width grid - 1)]) [0 .. (height grid - 1)]

deriving instance Eq a => Eq (Grid a)

instance Functor Grid where
  fmap f = Grid . fmap f . unGrid

fromList :: [[a]] -> Grid a
fromList [] = error "Empty grid."
fromList rows =
  let w = length (head rows)
      h = length rows
   in Grid (listArray ((0, 0), (h - 1, w - 1)) (concat rows))

fromDigits :: String -> Grid Int
fromDigits = fromList . map (map (read . pure)) . lines

width :: Grid a -> Int
width = succ . snd . snd . bounds . unGrid

height :: Grid a -> Int
height = succ . fst . snd . bounds . unGrid

lookup :: [(Int, Int)] -> Grid a -> [a]
lookup coordinates (Grid grid) = map ((grid Array.!) . Tuple.swap) coordinates

(!) :: Grid a -> (Int, Int) -> a
(!) grid coordinates = head $ lookup [coordinates] grid

allValues :: Grid a -> [a]
allValues grid = lookup (allCoordinates grid) grid

all :: (a -> Bool) -> Grid a -> Bool
all predicate = List.all predicate . allValues

(//) :: Grid a -> [((Int, Int), a)] -> Grid a
(//) (Grid grid) replacements = Grid (grid Array.// map (Bifunctor.first Tuple.swap) replacements)

updateWith :: (a -> a -> a) -> [((Int, Int), a)] -> Grid a -> Grid a
updateWith f updates grid = grid // map (\(c, x) -> (c, f (grid ! c) x)) updates

allCoordinates :: Grid a -> [(Int, Int)]
allCoordinates = range . bounds . unGrid

coordinatesWhere :: (a -> Bool) -> Grid a -> [(Int, Int)]
coordinatesWhere predicate grid = filter (predicate . (grid !)) (allCoordinates grid)

inBounds :: (Int, Int) -> Grid a -> Bool
inBounds coordinates (Grid grid) = inRange (bounds grid) coordinates

neighboringCoordinates :: (Int, Int) -> Grid a -> [(Int, Int)]
neighboringCoordinates (x, y) grid =
  filter (`inBounds` grid) [(x, y - 1), (x - 1, y), (x, y + 1), (x + 1, y)]

neighboringCoordinatesWithDiagonals :: (Int, Int) -> Grid a -> [(Int, Int)]
neighboringCoordinatesWithDiagonals (x, y) grid =
  filter (`inBounds` grid) [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x - 1, y), (x + 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

neighboringValues :: (Int, Int) -> Grid a -> [a]
neighboringValues coordinates grid =
  map (grid !) (neighboringCoordinates coordinates grid)
