import           Data.Array as Array
import qualified Data.Char as Char
import           Data.Ix (Ix)
import qualified Data.Ix as Ix
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type DuctMap = Array Coordinates Cell
type Coordinates = (Int, Int)
data Cell = Wall | Open | Start | ExposedWire Int
  deriving (Eq)

instance Show Cell where
  show Wall = "#"
  show Open = "."
  show (ExposedWire n) = show n

main = do
  input <- getContents
  let ductMap = parseInput input
  let startCoordinates = head $ filterArray (== Start) ductMap
  let wireCoordinates = exposedWires ductMap
  let coordinatePairs = map (normalize . toTuple) $ combinations 2 (startCoordinates : wireCoordinates)
  let distances = Map.fromList $ map (\cs -> (cs, distance ductMap cs)) coordinatePairs
  let distancePermutations = map (map ((distances Map.!) . normalize) . pairs . (startCoordinates :)) $ List.permutations wireCoordinates
  print $ head $ List.sort $ map sum distancePermutations

parseInput :: String -> DuctMap
parseInput input = toArray $ map (map parseCell) $ lines input
  where
  parseCell '#' = Wall
  parseCell '.' = Open
  parseCell '0' = Start
  parseCell cell
    | Char.isDigit cell = ExposedWire $ read [cell]
    | otherwise = error $ "Unknown cell: " ++ show cell
  toArray grid = listArray (listBounds grid) $ concat $ List.transpose grid
  listBounds grid = ((0, 0), (length (head grid) - 1, length grid - 1))

filterArray :: Ix index => (value -> Bool) -> Array index value -> [index]
filterArray predicate array = map fst $ List.filter (predicate . snd) (assocs array)

exposedWires :: DuctMap -> [Coordinates]
exposedWires = filterArray isExposedWire
  where
  isExposedWire (ExposedWire _) = True
  isExposedWire _ = False

normalize :: (Coordinates, Coordinates) -> (Coordinates, Coordinates)
normalize (from, to) = (min from to, max from to)

toTuple :: [a] -> (a, a)
toTuple [a, b] = (a, b)

distance :: DuctMap -> (Coordinates, Coordinates) -> Int
distance ductMap (from, to) = distance' Set.empty [(from, 0)]
  where
  walls = Set.fromList $ filterArray (== Wall) ductMap
  distance' past ((location@(x, y), soFar) : future)
    | location `Set.member` past = distance' past future
    | location == to = soFar
    | otherwise = distance' (Set.insert location past) (future ++ neighbours)
    where
    neighbours =
      map (\n -> (n, soFar + 1))
        $ filter (\n -> Ix.inRange (bounds ductMap) n && n `Set.notMember` walls && n `Set.notMember` past)
        $ [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [x] = []
pairs (a : b : xs) = (a, b) : pairs (b : xs)

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n list = [x : xs | x : ts <- List.tails list, xs <- combinations (n - 1) ts]

showMap :: DuctMap -> String
showMap grid =
  List.intercalate "\n" [concat [show (grid ! (x, y)) | x <- [minX..maxX]] | y <- [minY..maxY]]
  where
  ((minX, minY), (maxX, maxY)) = bounds grid
