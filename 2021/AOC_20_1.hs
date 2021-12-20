{-# OPTIONS -Wall #-}

import Control.Monad (when)
import qualified Data.Foldable as Foldable
import Data.Functor (($>))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Helpers.Grid (Grid)
import qualified Helpers.Grid as Grid
import Helpers.Numbers
import Helpers.Parse
import Helpers.Point (Point (..))
import qualified Helpers.Point as Point
import Text.Parsec

data Pixel = Dark | Light
  deriving (Eq, Ord)

instance Show Pixel where
  show Dark = "."
  show Light = "#"

main :: IO ()
main = do
  input <- Text.lines <$> Text.IO.getContents
  let enhancement = Vector.fromList $ parseText parser $ head input
  when (Vector.length enhancement /= 512) $ fail $ "Invalid enhancement length of " <> show (Vector.length enhancement) <> ".''"
  let image = Grid.toSparse Dark . Grid.fromList $ map (parseText parser) $ tail (tail input)
  let steps = map fst $ iterate (step enhancement) (image, Dark)
  print $ length $ Grid.allPointsList $ steps !! 2

step :: Vector Pixel -> (Grid Pixel, Pixel) -> (Grid Pixel, Pixel)
step enhancement (image, base) =
  (Grid.fromPoints newBase (Map.fromSet (const op) enhanced), newBase)
  where
    newBase = case base of Dark -> Vector.head enhancement; Light -> Vector.last enhancement
    op = opposite newBase
    points = Foldable.foldMap' expand $ Grid.allPoints image
    enhanced = Set.filter (\p -> enhance enhancement image p == op) points

expand :: Point -> Set Point
expand point = Set.insert point $ Point.neighboringPointsWithDiagonals point

enhance :: Vector Pixel -> Grid Pixel -> Point -> Pixel
enhance enhancement image point =
  enhancement Vector.! pixelsToInt pixels
  where
    pixels = map (image Grid.!) $ considered point

opposite :: Pixel -> Pixel
opposite Light = Dark
opposite Dark = Light

pixelsToInt :: [Pixel] -> Int
pixelsToInt = unBits . map toBool . reverse

toBool :: Pixel -> Bool
toBool Dark = False
toBool Light = True

considered :: Point -> [Point]
considered (Point y x) =
  [ Point (pred y) (pred x),
    Point (pred y) x,
    Point (pred y) (succ x),
    Point y (pred x),
    Point y x,
    Point y (succ x),
    Point (succ y) (pred x),
    Point (succ y) x,
    Point (succ y) (succ x)
  ]

parser :: Parsec Text () [Pixel]
parser = many pixel
  where
    pixel =
      choice
        [ try (char '.') $> Dark,
          try (char '#') $> Light
        ]
