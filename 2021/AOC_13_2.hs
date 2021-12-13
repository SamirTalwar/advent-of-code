{-# OPTIONS -Wall #-}

import Data.Functor
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Helpers.Function
import Helpers.Grid (Grid)
import qualified Helpers.Grid as Grid
import Helpers.Parse
import Helpers.Point (Point (..))
import Text.Parsec

data Mark = O | X
  deriving (Eq)

instance Show Mark where
  show O = " "
  show X = "#"

data FoldInstruction = FoldAlongX Int | FoldAlongY Int
  deriving (Eq, Show)

main :: IO ()
main = do
  (points, foldInstructions) <- parseTextIO parser
  let paper = Grid.fromPoints O (Map.fromSet (const X) points)
  let foldedPaper = foldl foldPaper paper foldInstructions
  print foldedPaper

foldPaper :: Grid Mark -> FoldInstruction -> Grid Mark
foldPaper grid (FoldAlongX x) =
  let maxY = Grid.height grid - 1
      maxX = Grid.width grid - 1
      left =
        grid
          |> Grid.subGrid (Point 0 0) (Point maxY (pred x))
          |> Grid.pointsWhere (== X)
      right =
        grid
          |> Grid.subGrid (Point 0 (succ x)) (Point maxY maxX)
          |> Grid.mapPoints (\(Point py px) -> Point py (x - (px - x)))
          |> Grid.pointsWhere (== X)
   in Grid.fromPoints O (Map.fromSet (const X) (left `Set.union` right))
foldPaper grid (FoldAlongY y) =
  let maxY = Grid.height grid - 1
      maxX = Grid.width grid - 1
      top =
        grid
          |> Grid.subGrid (Point 0 0) (Point (pred y) maxX)
          |> Grid.pointsWhere (== X)
      bottom =
        grid
          |> Grid.subGrid (Point (succ y) 0) (Point maxY maxX)
          |> Grid.mapPoints (\(Point py px) -> Point (y - (py - y)) px)
          |> Grid.pointsWhere (== X)
   in Grid.fromPoints O (Map.fromSet (const X) (top `Set.union` bottom))

parser :: Parsec Text () (Set Point, [FoldInstruction])
parser = do
  points <- Set.fromList <$> linesP (try point)
  _ <- string "\n"
  foldInstructions <- linesP foldInstruction
  return (points, foldInstructions)
  where
    point = do
      x <- int
      _ <- string ","
      y <- int
      return $ Point y x
    foldInstruction = do
      _ <- string "fold along "
      direction <- try (string "x" $> FoldAlongX) <|> try (string "y" $> FoldAlongY)
      _ <- string "="
      direction <$> int
