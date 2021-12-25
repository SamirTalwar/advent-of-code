{-# OPTIONS -Wall #-}

import Data.Functor
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import Helpers.Grid (Grid, (!), (//))
import qualified Helpers.Grid as Grid
import Helpers.Parse
import Helpers.Point (Point (..))
import Text.Parsec

data SeaCucumber = FacingEast | FacingSouth
  deriving (Eq)

instance Show SeaCucumber where
  show FacingEast = "> "
  show FacingSouth = "v "

type SeaFloor = Grid (Maybe SeaCucumber)

main :: IO ()
main = do
  seaFloor <- Grid.fromList <$> parseLinesIO parser
  let steps = iterate step seaFloor
  let answer = Maybe.fromJust (List.elemIndex True (zipWith (==) steps (tail steps))) + 1
  print answer

step :: SeaFloor -> SeaFloor
step = stepSouth . stepEast

stepEast :: SeaFloor -> SeaFloor
stepEast = stepInDirection nextPoint FacingEast
  where
    nextPoint (Point _ minX, Point _ maxX) (Point y x) = Point y (inc minX maxX x)

stepSouth :: SeaFloor -> SeaFloor
stepSouth = stepInDirection nextPoint FacingSouth
  where
    nextPoint (Point minY _, Point maxY _) (Point y x) = Point (inc minY maxY y) x

stepInDirection :: ((Point, Point) -> Point -> Point) -> SeaCucumber -> SeaFloor -> SeaFloor
stepInDirection updatePoint toMove seaFloor =
  let bounds = Grid.bounds seaFloor
      points = Grid.allPointsList seaFloor
      values = zip points $ map (seaFloor !) points
      available = Set.fromList $ map fst $ filter (Maybe.isNothing . snd) values
      candidates = filter ((== Just toMove) . snd) values
      canMove =
        filter (\(_, new, _) -> new `Set.member` available) $
          map (\(point, seaCucumber) -> (point, updatePoint bounds point, seaCucumber)) candidates
      updates = map (\(old, _, _) -> (old, Nothing)) canMove ++ map (\(_, new, seaCucumber) -> (new, seaCucumber)) canMove
   in seaFloor // updates

inc :: Int -> Int -> Int -> Int
inc lowest highest value = (value + 1) `mod` (highest - lowest + 1) + lowest

parser :: Parsec Text () [Maybe SeaCucumber]
parser = many1 seaCucumber
  where
    seaCucumber =
      try (char '>' $> Just FacingEast)
        <|> try (char 'v' $> Just FacingSouth)
        <|> (char '.' $> Nothing)
