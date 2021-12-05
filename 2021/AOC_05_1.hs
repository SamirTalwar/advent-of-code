{-# OPTIONS -Wall #-}

import qualified Data.Map as Map
import Helpers.Applicative
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
  let validLines = filter (isHorizontal <||> isVertical) input
  let allCoordinates = concatMap allCoordinatesInLine validLines
  let counts = countValues allCoordinates
  print $ Map.size $ Map.filter (> 1) counts

allCoordinatesInLine :: Line -> [Coordinate]
allCoordinatesInLine (Line (Coordinate x1 y1) (Coordinate x2 y2))
  | x1 == x2 = map (Coordinate x1) (if y1 <= y2 then [y1 .. y2] else [y2 .. y1])
  | y1 == y2 = map (`Coordinate` y1) (if x1 <= x2 then [x1 .. x2] else [x2 .. x1])
  | otherwise = error "Invalid line."

isHorizontal :: Line -> Bool
isHorizontal (Line (Coordinate x1 _) (Coordinate x2 _)) = x1 == x2

isVertical :: Line -> Bool
isVertical (Line (Coordinate _ y1) (Coordinate _ y2)) = y1 == y2

parseInput :: IO [Line]
parseInput = parseLinesIO line
  where
    line = Line <$> coordinate <*> (string " -> " *> coordinate)
    coordinate = Coordinate <$> int <*> (string "," *> int)
