{-# OPTIONS -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}

module Helpers.Matrix
  ( Matrix,
    fromList,
    width,
    height,
    lookup,
    (!),
    allCoordinates,
    inBounds,
    neighboringCoordinates,
    neighboringValues,
  )
where

import Data.Array hiding ((!))
import qualified Data.Array as Array
import Prelude hiding (lookup)

newtype Matrix a = Matrix {unMatrix :: Array (Int, Int) a}

deriving instance Eq a => Eq (Matrix a)

deriving instance Show a => Show (Matrix a)

fromList :: [[a]] -> Matrix a
fromList [] = error "Empty matrix."
fromList rows =
  let w = length (head rows)
      h = length rows
   in Matrix (listArray ((0, 0), (h - 1, w - 1)) (concat rows))

width :: Matrix a -> Int
width = succ . snd . snd . bounds . unMatrix

height :: Matrix a -> Int
height = succ . fst . snd . bounds . unMatrix

lookup :: Int -> Int -> Matrix a -> a
lookup x y (Matrix matrix) = matrix Array.! (y, x)

(!) :: Matrix a -> (Int, Int) -> a
(!) matrix (x, y) = lookup x y matrix

allCoordinates :: Matrix a -> [(Int, Int)]
allCoordinates = range . bounds . unMatrix

inBounds :: (Int, Int) -> Matrix a -> Bool
inBounds coordinates (Matrix matrix) = inRange (bounds matrix) coordinates

neighboringCoordinates :: (Int, Int) -> Matrix a -> [(Int, Int)]
neighboringCoordinates (x, y) matrix =
  filter (`inBounds` matrix) [(x, y - 1), (x - 1, y), (x, y + 1), (x + 1, y)]

neighboringValues :: (Int, Int) -> Matrix a -> [a]
neighboringValues coordinates matrix =
  map (matrix !) (neighboringCoordinates coordinates matrix)
