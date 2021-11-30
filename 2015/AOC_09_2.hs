import Data.Foldable (toList)
import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text

type Distance = ((Location, Location), Int)

type Distances = Map (Location, Location) Int

newtype Location = Location String
  deriving (Eq, Ord, Show)

main = do
  distances <- Map.fromList <$> map parseInput <$> Text.lines <$> IO.getContents
  let locations = Set.fromList $ concatMap (\(from, to) -> List.sort [from, to]) $ Map.keys distances
  let locationPermutations = map sortedPairs $ List.permutations $ toList locations
  let distancePermutations = solve distances locationPermutations
  print $ maximum $ map sum distancePermutations

parseInput :: Text -> Distance
parseInput text = either (error . show) id $ parse parser "" text
  where
    parser = do
      from <- location
      string " to "
      to <- location
      string " = "
      distance <- number
      return $ (sortTuple (from, to), distance)
    location = Location <$> many1 letter
    number = read <$> many1 digit

solve :: Distances -> [[(Location, Location)]] -> [[Int]]
solve distances =
  map Maybe.catMaybes
    . filter (all Maybe.isJust)
    . map (map (`Map.lookup` distances))

sortedPairs :: Ord a => [a] -> [(a, a)]
sortedPairs [] = []
sortedPairs [x] = []
sortedPairs (a : xs@(b : _)) = sortTuple (a, b) : sortedPairs xs

sortTuple :: Ord a => (a, a) -> (a, a)
sortTuple (a, b)
  | a <= b = (a, b)
  | otherwise = (b, a)
