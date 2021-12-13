{-# OPTIONS -Wall #-}

import Data.Bifunctor (first)
import Data.Functor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
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
  deriving (Eq, Ord)

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
  let answer = parseGridText foldedPaper
  putStrLn answer

foldPaper :: Grid Mark -> FoldInstruction -> Grid Mark
foldPaper grid (FoldAlongX x) =
  let Point maxY maxX = snd $ Grid.bounds grid
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
  let Point maxY maxX = snd $ Grid.bounds grid
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

parseGridText :: Grid Mark -> String
parseGridText grid =
  if null grid
    then []
    else
      let firstLetter = Grid.subGrid (Point 0 0) (Point 5 3) grid
          Point _ maxX = snd $ Grid.bounds grid
          rest = Grid.mapPoints (\(Point y x) -> Point y (x - 5)) $ Grid.subGrid (Point 0 5) (Point 5 maxX) grid
       in parseGridLetter firstLetter : parseGridText rest
  where
    parseGridLetter :: Grid Mark -> Char
    parseGridLetter letterGrid = Maybe.fromMaybe (error ("Unknown character:\n" ++ show letterGrid)) (Map.lookup letterGrid letters)

-- I only encoded the letters in my answer; I don't know what the rest would look like.
letters :: Map (Grid Mark) Char
letters =
  Map.fromList $
    map
      (first Grid.fromList)
      [ ( [ [O, X, X, O],
            [X, O, O, X],
            [X, O, O, X],
            [X, X, X, X],
            [X, O, O, X],
            [X, O, O, X]
          ],
          'A'
        ),
        ( [ [O, X, X, O],
            [X, O, O, X],
            [X, O, O, O],
            [X, O, O, O],
            [X, O, O, X],
            [O, X, X, O]
          ],
          'C'
        ),
        ( [ [X, X, X, X],
            [X, O, O, O],
            [X, X, X, O],
            [X, O, O, O],
            [X, O, O, O],
            [X, O, O, O]
          ],
          'F'
        ),
        ( [ [X, O, O, X],
            [X, O, O, X],
            [X, X, X, X],
            [X, O, O, X],
            [X, O, O, X],
            [X, O, O, X]
          ],
          'H'
        ),
        ( [ [O, O, X, X],
            [O, O, O, X],
            [O, O, O, X],
            [O, O, O, X],
            [X, O, O, X],
            [O, X, X, O]
          ],
          'J'
        ),
        ( [ [X, O, O, X],
            [X, O, X, O],
            [X, X, O, O],
            [X, O, X, O],
            [X, O, X, O],
            [X, O, O, X]
          ],
          'K'
        ),
        ( [ [X, X, X, X],
            [O, O, O, X],
            [O, O, X, O],
            [O, X, O, O],
            [X, O, O, O],
            [X, X, X, X]
          ],
          'Z'
        )
      ]
