{-# OPTIONS -Wall #-}

import Control.Monad (when)
import qualified Data.Foldable as Foldable
import Data.Functor (($>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text.IO
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Helpers.Function
import Helpers.Numbers (unBits)
import Helpers.Parse
import Helpers.Point (Point (..))
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
  let pixels = map (parseText parser) $ tail (tail input)
  let image = parseImagePoints pixels
  let steps = map fst $ iterate (step enhancement) (image, Dark)
  print $ Set.size $ steps !! 50

parseImagePoints :: [[Pixel]] -> Set Point
parseImagePoints pixels =
  pixels
    |> zipWith (\y row -> zipWith (\x pixel -> (Point y x, pixel)) [0 ..] row) [0 ..]
    |> concat
    |> filter ((== Light) . snd)
    |> map fst
    |> Set.fromList

step :: Vector Pixel -> (Set Point, Pixel) -> (Set Point, Pixel)
step enhancement (image, base) =
  (enhanced, newBase)
  where
    newBase = case base of Dark -> Vector.head enhancement; Light -> Vector.last enhancement
    op = opposite newBase
    points = Foldable.foldMap' (Set.fromList . considered) image
    enhanced = Set.filter (\p -> enhance enhancement base image p == op) points

enhance :: Vector Pixel -> Pixel -> Set Point -> Point -> Pixel
enhance enhancement base image point =
  enhancement Vector.! pixelsToInt pixels
  where
    pixels = map (\p -> if p `Set.member` image then opposite base else base) (considered point)

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
