{-# OPTIONS -Wall #-}

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Helpers.Function
import Helpers.Numbers
import Helpers.Parse
import Helpers.Point (Point (..))
import qualified Helpers.Point as Point
import Text.Parsec

data TargetArea = TargetArea Point Point

main :: IO ()
main = do
  targetArea <- parseTextIO parser
  let attempts = possibleInitialVelocities targetArea
  let results = Maybe.mapMaybe (validTrajectory targetArea) attempts
  let answer = maximum $ concatMap (map pY) results
  print answer

validTrajectory :: TargetArea -> Point -> Maybe [Point]
validTrajectory targetArea@(TargetArea (Point yStart _) _) initialVelocity =
  let (before, after) =
        iterate step (initialVelocity, Point 0 0)
          |> map snd
          |> takeWhile ((>= yStart) . pY)
          |> List.break (inTargetArea targetArea)
   in if null after then Nothing else Just (before ++ [head after])

possibleInitialVelocities :: TargetArea -> [Point]
possibleInitialVelocities (TargetArea (Point yStart xStart) (Point _ xEnd)) =
  let xs = takeWhile (\x -> triangular x <= xEnd) $ dropWhile (\x -> triangular x < xStart) [0 ..]
      ys = [0 .. (- yStart)]
   in [Point y x | y <- ys, x <- xs]

step :: (Point, Point) -> (Point, Point)
step (trajectory, position) = (updateTrajectory trajectory, position <> trajectory)

updateTrajectory :: Point -> Point
updateTrajectory (Point y x) = Point (y - 1) x'
  where
    x' = case compare x 0 of
      EQ -> 0
      LT -> x + 1
      GT -> x - 1

inTargetArea :: TargetArea -> Point -> Bool
inTargetArea (TargetArea start end) = Point.withinBounds (start, end)

parser :: Parsec Text () TargetArea
parser = do
  _ <- string "target area: x="
  xStart <- int
  _ <- string ".."
  xEnd <- int
  _ <- string ", y="
  yStart <- int
  _ <- string ".."
  yEnd <- int
  return $ TargetArea (Point yStart xStart) (Point yEnd xEnd)
