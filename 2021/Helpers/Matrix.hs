{-# OPTIONS -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}

module Helpers.Matrix
  ( Matrix,
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

newtype Matrix a = Matrix {unMatrix :: Array (Int, Int) a}

instance Show a => Show (Matrix a) where
  show matrix =
    List.intercalate "\n" $ map (\y -> unwords $ map (\x -> show (matrix ! (x, y))) [0 .. (width matrix - 1)]) [0 .. (height matrix - 1)]

deriving instance Eq a => Eq (Matrix a)

instance Functor Matrix where
  fmap f (Matrix matrix) = Matrix (fmap f matrix)

fromList :: [[a]] -> Matrix a
fromList [] = error "Empty matrix."
fromList rows =
  let w = length (head rows)
      h = length rows
   in Matrix (listArray ((0, 0), (h - 1, w - 1)) (concat rows))

fromDigits :: String -> Matrix Int
fromDigits = fromList . map (map (read . pure)) . lines

width :: Matrix a -> Int
width = succ . snd . snd . bounds . unMatrix

height :: Matrix a -> Int
height = succ . fst . snd . bounds . unMatrix

lookup :: [(Int, Int)] -> Matrix a -> [a]
lookup coordinates (Matrix matrix) = map ((matrix Array.!) . Tuple.swap) coordinates

(!) :: Matrix a -> (Int, Int) -> a
(!) matrix coordinates = head $ lookup [coordinates] matrix

allValues :: Matrix a -> [a]
allValues matrix = lookup (allCoordinates matrix) matrix

all :: (a -> Bool) -> Matrix a -> Bool
all predicate = List.all predicate . allValues

(//) :: Matrix a -> [((Int, Int), a)] -> Matrix a
(//) (Matrix matrix) replacements = Matrix (matrix Array.// map (Bifunctor.first Tuple.swap) replacements)

updateWith :: (a -> a -> a) -> [((Int, Int), a)] -> Matrix a -> Matrix a
updateWith f updates matrix = matrix // map (\(c, x) -> (c, f (matrix ! c) x)) updates

allCoordinates :: Matrix a -> [(Int, Int)]
allCoordinates = range . bounds . unMatrix

coordinatesWhere :: (a -> Bool) -> Matrix a -> [(Int, Int)]
coordinatesWhere predicate matrix = filter (predicate . (matrix !)) (allCoordinates matrix)

inBounds :: (Int, Int) -> Matrix a -> Bool
inBounds coordinates (Matrix matrix) = inRange (bounds matrix) coordinates

neighboringCoordinates :: (Int, Int) -> Matrix a -> [(Int, Int)]
neighboringCoordinates (x, y) matrix =
  filter (`inBounds` matrix) [(x, y - 1), (x - 1, y), (x, y + 1), (x + 1, y)]

neighboringCoordinatesWithDiagonals :: (Int, Int) -> Matrix a -> [(Int, Int)]
neighboringCoordinatesWithDiagonals (x, y) matrix =
  filter (`inBounds` matrix) [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1), (x - 1, y), (x + 1, y), (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

neighboringValues :: (Int, Int) -> Matrix a -> [a]
neighboringValues coordinates matrix =
  map (matrix !) (neighboringCoordinates coordinates matrix)
