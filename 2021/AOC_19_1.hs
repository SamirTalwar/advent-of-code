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
import Helpers.Memoization
import Helpers.Parse
import Text.Parsec hiding ((<|>))

newtype Scanner = Scanner (Set Beacon)
  deriving (Show)

instance HasTrie Scanner where
  data Scanner :->: b = ScannerTrie (Set Beacon :->: b)
  trie f = ScannerTrie $ trie $ f . Scanner
  unTrie (ScannerTrie f) (Scanner beacons) = unTrie f beacons

data Beacon = Beacon Int16 Int16 Int16
  deriving (Eq, Ord)

instance HasTrie Beacon where
  data Beacon :->: b = BeaconTrie ((Int16, Int16, Int16) :->: b)
  trie f = BeaconTrie $ trie $ \(x, y, z) -> f (Beacon x y z)
  unTrie (BeaconTrie f) (Beacon x y z) = unTrie f (x, y, z)

instance Show Beacon where
  show (Beacon x y z) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"

data Direction = Pos Coordinate | Neg Coordinate

data Coordinate = X | Y | Z

main :: IO ()
main = do
  scanners <- parseTextIO parser
  let reoriented = map (\(Scanner beacons) -> beacons) $ reorient scanners
  let allBeacons = foldr Set.union Set.empty reoriented
  print $ Set.size allBeacons

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
    overlaps n (Scanner as) (Scanner bs) =
      Set.size (as `Set.intersection` bs) >= n

translate :: Scanner -> Scanner -> [Scanner]
translate (Scanner as) (Scanner bs) = do
  a <- Set.toList as
  b <- Set.toList bs
  let (dX, dY, dZ) = b `diff` a
  return $ Scanner (Set.map (\(Beacon x y z) -> Beacon (x - dX) (y - dY) (z - dZ)) bs)
  where
    diff (Beacon aX aY aZ) (Beacon bX bY bZ) = (aX - bX, aY - bY, aZ - bZ)

rotate :: Scanner -> [Scanner]
rotate (Scanner beacons) = map (\d -> Scanner $ Set.map (rotate' d) beacons) rotations
  where
    rotate' (dX, dY, dZ) beacon =
      Beacon (getDirection dX beacon) (getDirection dY beacon) (getDirection dZ beacon)
    getDirection (Pos X) (Beacon x _ _) = x
    getDirection (Neg X) (Beacon x _ _) = - x
    getDirection (Pos Y) (Beacon _ y _) = y
    getDirection (Neg Y) (Beacon _ y _) = - y
    getDirection (Pos Z) (Beacon _ _ z) = z
    getDirection (Neg Z) (Beacon _ _ z) = - z

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

parser :: Parsec Text () [Scanner]
parser = sepBy scanner (string "\n")
  where
    scanner = do
      _ <- string "--- scanner " *> int <* string " ---\n"
      beacons <- Set.fromList <$> many beacon
      return $ Scanner beacons
    beacon = do
      x <- fromIntegral <$> int
      _ <- char ','
      y <- fromIntegral <$> int
      _ <- char ','
      z <- fromIntegral <$> int
      _ <- char '\n'
      return $ Beacon x y z
