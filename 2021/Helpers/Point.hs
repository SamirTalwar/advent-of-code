{-# OPTIONS -Wall #-}

module Helpers.Point
  ( Point (..),
    withinBounds,
    allPointsWithinBounds,
    neighboringPoints,
    neighboringPointsWithDiagonals,
  )
where

import Data.Ix
import Data.Set (Set)
import qualified Data.Set as Set

data Point = Point {pY :: Int, pX :: Int}
  deriving (Eq, Ord, Bounded, Ix)

instance Show Point where
  show (Point y x) = "(" <> show x <> ", " <> show y <> ")"

instance Semigroup Point where
  Point y1 x1 <> Point y2 x2 = Point (y1 + y2) (x1 + x2)

instance Monoid Point where
  mempty = Point 0 0

withinBounds :: (Point, Point) -> Point -> Bool
withinBounds (Point yStart xStart, Point yEnd xEnd) (Point y x) =
  y >= yStart && y <= yEnd && x >= xStart && x <= xEnd

allPointsWithinBounds :: Int -> Int -> [Point]
allPointsWithinBounds h w = [Point y x | x <- [0 .. w - 1], y <- [0 .. h - 1]]

neighboringPoints :: Point -> Set Point
neighboringPoints point = Set.map (point <>) cardinalPoints

neighboringPointsWithDiagonals :: Point -> Set Point
neighboringPointsWithDiagonals point = Set.map (point <>) pointsInAllDirections

cardinalPoints :: Set Point
cardinalPoints =
  Set.fromList
    [ Point (-1) 0,
      Point 0 (-1),
      Point 0 1,
      Point 1 0
    ]

diagonalPoints :: Set Point
diagonalPoints =
  Set.fromList
    [ Point (-1) (-1),
      Point (-1) 1,
      Point 1 (-1),
      Point 1 1
    ]

pointsInAllDirections :: Set Point
pointsInAllDirections = cardinalPoints <> diagonalPoints
