{-# OPTIONS -Wall #-}

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Helpers.Map
import Helpers.Parse
import Text.Parsec hiding (Line)

data Coordinate = Coordinate Int Int
  deriving (Eq, Ord, Show)

data Line = Line Coordinate Coordinate
  deriving (Show)

main :: IO ()
main = do
  input <- map parseInput . Text.lines <$> IO.getContents
  let allCoordinates = concatMap allCoordinatesInLine input
  let counts = countValues allCoordinates
  print $ Map.size $ Map.filter (> 1) counts

allCoordinatesInLine :: Line -> [Coordinate]
allCoordinatesInLine (Line (Coordinate x1 y1) (Coordinate x2 y2))
  | x1 == x2 = map (Coordinate x1) (if y1 <= y2 then [y1 .. y2] else [y2 .. y1])
  | y1 == y2 = map (`Coordinate` y1) (if x1 <= x2 then [x1 .. x2] else [x2 .. x1])
  | otherwise =
    let xs = if x1 <= x2 then [x1 .. x2] else reverse [x2 .. x1]
        ys = if y1 <= y2 then [y1 .. y2] else reverse [y2 .. y1]
     in zipWith Coordinate xs ys

parseInput :: Text -> Line
parseInput = parseText line
  where
    line = Line <$> coordinate <*> (string " -> " *> coordinate)
    coordinate = Coordinate <$> int <*> (string "," *> int)
