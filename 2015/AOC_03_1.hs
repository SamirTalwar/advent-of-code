import qualified Data.Char as Char
import qualified Data.Set as Set

data Direction = North | East | South | West
  deriving (Eq, Show)

type Coordinates = (Int, Int)

main = do
  directions <- parseInput <$> trim <$> getContents
  let visitedLocations = Set.fromList $ scanl (flip apply) (0, 0) directions
  print $ Set.size visitedLocations

trim :: String -> String
trim = takeWhile (not . Char.isSpace) . dropWhile Char.isSpace

parseInput :: String -> [Direction]
parseInput = map parseChar
  where
    parseChar '^' = North
    parseChar '>' = East
    parseChar 'v' = South
    parseChar '<' = West

apply :: Direction -> Coordinates -> Coordinates
apply North (x, y) = (x, y + 1)
apply East (x, y) = (x + 1, y)
apply South (x, y) = (x, y - 1)
apply West (x, y) = (x - 1, y)
