{-# LANGUAGE TupleSections #-}

{-# OPTIONS -Wall #-}

import Control.Applicative ((<|>))
import qualified Data.Either as Either
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Helpers.Parse
import Text.Parsec hiding ((<|>))

data Scanner = Scanner Int Point (Set Beacon)
  deriving (Show)

newtype Beacon = Beacon Point
  deriving (Eq, Ord)

instance Show Beacon where
  show (Beacon point) = show point

data Point = Point Int Int Int
  deriving (Eq, Ord)

instance Show Point where
  show (Point x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

data Direction = Pos Coordinate | Neg Coordinate

data Coordinate = X | Y | Z

main :: IO ()
main = do
  scanners <- parseTextIO parser
  let reoriented = reorient scanners
  let distances = map (\(Scanner _ aPos _, Scanner _ bPos _) -> manhattanDistance (bPos `diff` aPos)) $ selectPairs reoriented
  print $ maximum distances

reorient :: [Scanner] -> [Scanner]
reorient [] = []
reorient (first : rest) = reorient' [first] rest
  where
    reorient' oriented [] = oriented
    reorient' oriented scanners =
      let rotatedScanners = map (\s -> maybe (Left s) Right $ List.foldl' (\x o -> x <|> matchBeacons o (concatMap (translate o) (rotate s))) Nothing oriented) scanners
          (remaining, reoriented) = Either.partitionEithers rotatedScanners
       in if null reoriented
            then error ("Could not orient these scanners: " <> show remaining)
            else reorient' (oriented ++ reoriented) remaining

matchBeacons :: Scanner -> [Scanner] -> Maybe Scanner
matchBeacons against = List.find (overlaps 12 against)
  where
    overlaps n (Scanner _ _ as) (Scanner _ _ bs) =
      Set.size (as `Set.intersection` bs) >= n

translate :: Scanner -> Scanner -> [Scanner]
translate (Scanner _ _ as) (Scanner n position bs) = do
  Beacon a <- Set.toList as
  Beacon b <- Set.toList bs
  let d = b `diff` a
  return $ Scanner n (position `diff` d) (Set.map (\(Beacon beaconPosition) -> Beacon (beaconPosition `diff` d)) bs)

rotate :: Scanner -> [Scanner]
rotate (Scanner n position beacons) = map (\d -> Scanner n position $ Set.map (rotate' d) beacons) rotations
  where
    rotate' (dX, dY, dZ) (Beacon point) =
      Beacon (Point (getDirection dX point) (getDirection dY point) (getDirection dZ point))
    getDirection (Pos X) (Point x _ _) = x
    getDirection (Neg X) (Point x _ _) = - x
    getDirection (Pos Y) (Point _ y _) = y
    getDirection (Neg Y) (Point _ y _) = - y
    getDirection (Pos Z) (Point _ _ z) = z
    getDirection (Neg Z) (Point _ _ z) = - z

rotations :: [(Direction, Direction, Direction)]
rotations =
  [ (Neg Z, Pos Y, Pos X),
    (Pos Y, Pos Z, Pos X),
    (Pos Z, Neg Y, Pos X),
    (Neg Y, Neg Z, Pos X),
    (Pos Z, Pos Y, Neg X),
    (Neg Y, Pos Z, Neg X),
    (Neg Z, Neg Y, Neg X),
    (Pos Y, Neg Z, Neg X),
    (Pos X, Neg Z, Pos Y),
    (Neg Z, Neg X, Pos Y),
    (Neg X, Pos Z, Pos Y),
    (Pos Z, Pos X, Pos Y),
    (Neg X, Neg Z, Neg Y),
    (Pos Z, Neg X, Neg Y),
    (Pos X, Pos Z, Neg Y),
    (Neg Z, Pos X, Neg Y),
    (Pos X, Pos Y, Pos Z),
    (Pos Y, Neg X, Pos Z),
    (Neg X, Neg Y, Pos Z),
    (Neg Y, Pos X, Pos Z),
    (Neg X, Pos Y, Neg Z),
    (Neg Y, Neg X, Neg Z),
    (Pos X, Neg Y, Neg Z),
    (Pos Y, Pos X, Neg Z)
  ]

diff :: Point -> Point -> Point
diff (Point aX aY aZ) (Point bX bY bZ) = Point (aX - bX) (aY - bY) (aZ - bZ)

manhattanDistance :: Point -> Int
manhattanDistance (Point x y z) = abs x + abs y + abs z

selectPairs :: [a] -> [(a, a)]
selectPairs xs = concat $ zipWith (\i x -> let (before, after) = List.splitAt i xs in map (x,) (before ++ tail after)) [0 ..] xs

parser :: Parsec Text () [Scanner]
parser = sepBy scanner (string "\n")
  where
    scanner = do
      number <- string "--- scanner " *> int <* string " ---\n"
      beacons <- Set.fromList <$> many beacon
      return $ Scanner number (Point 0 0 0) beacons
    beacon = do
      x <- int
      _ <- char ','
      y <- int
      _ <- char ','
      z <- int
      _ <- char '\n'
      return $ Beacon (Point x y z)
