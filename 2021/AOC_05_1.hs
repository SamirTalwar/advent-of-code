{-# OPTIONS -Wall #-}

import qualified Data.Map as Map
import Helpers.Map
import Helpers.Parse
import Text.Parsec hiding (Line)

data Coordinate = Coordinate Int Int
  deriving (Eq, Ord, Show)

data Line = Line Coordinate Coordinate
  deriving (Show)

main :: IO ()
main = do
  input <- parseInput
  let allCoordinates = concatMap allCoordinatesInLine input
  let counts = countValues allCoordinates
  print $ Map.size $ Map.filter (> 1) counts

allCoordinatesInLine :: Line -> [Coordinate]
allCoordinatesInLine (Line (Coordinate x1 y1) (Coordinate x2 y2))
  | x1 == x2 = [Coordinate x1 y | y <- if y1 <= y2 then [y1 .. y2] else [y2 .. y1]]
  | y1 == y2 = [Coordinate x y1 | x <- if x1 <= x2 then [x1 .. x2] else [x2 .. x1]]
  | otherwise = []

parseInput :: IO [Line]
parseInput = parseLinesIO line
  where
    line = Line <$> coordinate <*> (string " -> " *> coordinate)
    coordinate = Coordinate <$> int <*> (string "," *> int)
