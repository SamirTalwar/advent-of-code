{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import qualified Data.Either as Either
import qualified Data.Foldable as Foldable
import Data.Int (Int16)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Helpers.List
import Helpers.Memoization
import Helpers.Parse
import Text.Parsec hiding ((<|>))

data Scanner = Scanner Point (Set Beacon)
  deriving (Show)

instance HasTrie Scanner where
  data Scanner :->: b = ScannerTrie (Point :->: Set Beacon :->: b)
  trie f = ScannerTrie $ trie $ \point -> trie $ \beacons -> f (Scanner point beacons)
  unTrie (ScannerTrie f) (Scanner point beacons) = unTrie (unTrie f point) beacons

newtype Beacon = Beacon Point
  deriving (Eq, Ord)

instance HasTrie Beacon where
  data Beacon :->: b = BeaconTrie (Point :->: b)
  trie f = BeaconTrie $ trie $ f . Beacon
  unTrie (BeaconTrie f) (Beacon point) = unTrie f point

instance Show Beacon where
  show (Beacon point) = show point

data Point = Point Int16 Int16 Int16
  deriving (Eq, Ord)

instance Show Point where
  show (Point x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

instance HasTrie Point where
  data Point :->: b = PointTrie ((Int16, Int16, Int16) :->: b)
  trie f = PointTrie $ trie $ \(x, y, z) -> f (Point x y z)
  unTrie (PointTrie f) (Point x y z) = unTrie f (x, y, z)

data Direction = Pos Coordinate | Neg Coordinate

data Coordinate = X | Y | Z

main :: IO ()
main = do
  scanners <- parseTextIO parser
  let reoriented = reorient scanners
  let distances = map (\(Scanner aPos _, Scanner bPos _) -> manhattanDistance (bPos `diff` aPos)) $ selectPairs reoriented
  print $ maximum distances

reorient :: [Scanner] -> [Scanner]
reorient [] = []
reorient (first : rest) = reorient' [first] rest
  where
    reorient' oriented [] = oriented
    reorient' oriented scanners =
      let (remaining, reoriented) = Either.partitionEithers $ map (reorientScanner oriented) scanners
       in if null reoriented
            then error ("Could not orient these scanners: " <> show remaining)
            else reorient' (reoriented ++ oriented) remaining
    reorientScanner oriented scanner =
      maybe (Left scanner) Right $ Foldable.asum $ map (`memoMatchBeacons` scanner) oriented

memoMatchBeacons :: Scanner -> Scanner -> Maybe Scanner
memoMatchBeacons = memo2 matchBeacons

matchBeacons :: Scanner -> Scanner -> Maybe Scanner
matchBeacons against scanner = List.find (overlaps 12 against) aligned
  where
    aligned = rotate scanner >>= translate against
    overlaps n (Scanner _ as) (Scanner _ bs) =
      Set.size (as `Set.intersection` bs) >= n

translate :: Scanner -> Scanner -> [Scanner]
translate (Scanner _ as) (Scanner position bs) = do
  Beacon a <- Set.toList as
  Beacon b <- Set.toList bs
  let d = b `diff` a
  return $ Scanner (position `diff` d) (Set.map (\(Beacon beaconPosition) -> Beacon (beaconPosition `diff` d)) bs)

rotate :: Scanner -> [Scanner]
rotate (Scanner position beacons) = map (\d -> Scanner position $ Set.map (rotate' d) beacons) rotations
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

manhattanDistance :: Point -> Int16
manhattanDistance (Point x y z) = abs x + abs y + abs z

parser :: Parsec Text () [Scanner]
parser = sepBy scanner (string "\n")
  where
    scanner = do
      _ <- string "--- scanner " *> int <* string " ---\n"
      beacons <- Set.fromList <$> many beacon
      return $ Scanner (Point 0 0 0) beacons
    beacon = do
      x <- fromIntegral <$> int
      _ <- char ','
      y <- fromIntegral <$> int
      _ <- char ','
      z <- fromIntegral <$> int
      _ <- char '\n'
      return $ Beacon (Point x y z)
