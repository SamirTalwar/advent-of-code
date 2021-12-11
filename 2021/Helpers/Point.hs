{-# OPTIONS -Wall #-}

module Helpers.Point
  ( Point (..),
    neighboringPoints,
    neighboringPointsWithDiagonals,
  )
where

import Data.Ix

data Point = Point {pY :: Int, pX :: Int}
  deriving (Eq, Ord, Bounded, Ix)

instance Semigroup Point where
  Point y1 x1 <> Point y2 x2 = Point (y1 + y2) (x1 + x2)

instance Monoid Point where
  mempty = Point 0 0

neighboringPoints :: Point -> [Point]
neighboringPoints point = map (point <>) cardinalPoints

neighboringPointsWithDiagonals :: Point -> [Point]
neighboringPointsWithDiagonals point = map (point <>) pointsInAllDirections

cardinalPoints :: [Point]
cardinalPoints =
  [ Point (-1) 0,
    Point 0 (-1),
    Point 0 1,
    Point 1 0
  ]

diagonalPoints :: [Point]
diagonalPoints =
  [ Point (-1) (-1),
    Point (-1) 1,
    Point 1 (-1),
    Point 1 1
  ]

pointsInAllDirections :: [Point]
pointsInAllDirections = cardinalPoints ++ diagonalPoints
