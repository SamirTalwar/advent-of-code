{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.Array
import qualified Data.Ix as Ix
import qualified Data.List as List
import qualified Data.Ord as Ord
import           Data.Ratio
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import           Text.Parsec
import           Text.Parsec.Text

data Disk = Disk {
    location :: Coordinate,
    size :: Size,
    used :: Size
  } deriving (Eq, Ord, Show)
type Disks = Array Coordinate Disk
type Coordinate = (Int, Int)
type Movement = (Coordinate, Coordinate)
newtype Size = Terabyte Int
  deriving (Eq, Ord, Num)

instance Show Size where
  show (Terabyte magnitude) = show magnitude ++ "T"

normalSize = Terabyte 100

main = do
  input <- Text.lines <$> IO.getContents
  let disksList = map parseInput $ drop 2 input
  let locations = map location disksList
  let diskBounds = (head locations, last locations)
  let disks = listArray diskBounds disksList
  let source = topRight diskBounds
  let destination = fst diskBounds
  let solution = countMoves source destination disks
  print solution

parseInput :: Text -> Disk
parseInput text = either (error . show) id $ parse parser "" text
  where
  parser = do
    string "/dev/grid/node-x"
    x <- number
    string "-y"
    y <- number
    whitespace
    diskSize <- size
    whitespace
    used <- size
    whitespace
    size
    whitespace
    percentage
    return $ Disk (x, y) diskSize used
  size = do
    magnitude <- number
    char 'T'
    return $ Terabyte magnitude
  percentage = do
    magnitude <- number
    char '%'
    return (magnitude % 100)
  number = read <$> many1 digit
  whitespace = many1 space

topRight :: (Coordinate, Coordinate) -> Coordinate
topRight ((_, _), (highestX, highestY)) = (highestX, 0)

countMoves :: Coordinate -> Coordinate -> Disks -> Int
countMoves source destination disks =
  distance (location emptyDisk) leftOfSource disks
    + 1
    + 5 * naiveDistance destination leftOfSource
  where
  leftOfSource = leftOf source
  leftOf (x, y) = (x - 1, y)
  (Just emptyDisk) = List.find ((== Terabyte 0) . used) $ elems disks

distance :: Coordinate -> Coordinate -> Disks -> Int
distance from to disks = distance' Set.empty [(from, 0)]
  where
  massiveDisks = Set.fromList $ map location $ filter massive $ elems disks
  distance' past ((location@(x, y), soFar) : future)
    | location `Set.member` past = distance' past future
    | location == to = soFar
    | otherwise = distance' (Set.insert location past) (future ++ neighbours)
    where
    neighbours =
      map (\n -> (n, soFar + 1))
        $ filter (\n -> Ix.inRange (bounds disks) n && n `Set.notMember` massiveDisks && n `Set.notMember` past)
        $ [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

naiveDistance :: Coordinate -> Coordinate -> Int
naiveDistance (aX, aY) (bX, bY) = abs (bX - aX) + abs (bY - aY)

massive :: Disk -> Bool
massive disk = size disk > normalSize

showGrid :: Coordinate -> Coordinate -> Disks -> String
showGrid source destination disks =
  List.intercalate "\n" [List.intersperse ' ' [showDisk (disks ! (x, y)) | x <- [0..maxX]] | y <- [0..maxY]]
  where
  showDisk disk
    | location disk == source = 'G'
    | location disk == destination = '!'
    | used disk == 0 = '_'
    | used disk > normalSize = '#'
    | otherwise = '.'
  (maxX, maxY) = snd $ bounds disks
