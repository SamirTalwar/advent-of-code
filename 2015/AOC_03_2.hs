import qualified Data.Char as Char
import qualified Data.Set as Set

data Direction = North | East | South | West
  deriving (Eq, Show)

type Coordinates = (Int, Int)

main = do
  directions <- parseInput <$> trim <$> getContents
  let (santaDirections, roboSantaDirections) = alternate directions
  let visitedSantaLocations = scanl (flip apply) (0, 0) santaDirections
  let visitedRoboSantaLocations = scanl (flip apply) (0, 0) roboSantaDirections
  print $ Set.size $ Set.fromList $ visitedSantaLocations ++ visitedRoboSantaLocations

trim :: String -> String
trim = takeWhile (not . Char.isSpace) . dropWhile Char.isSpace

parseInput :: String -> [Direction]
parseInput = map parseChar
  where
  parseChar '^' = North
  parseChar '>' = East
  parseChar 'v' = South
  parseChar '<' = West

alternate :: [a] -> ([a], [a])
alternate [] = ([], [])
alternate (x : xs) =
  let (as, bs) = alternate' xs
  in (x : as, bs)
  where
  alternate' (x : xs) =
    let (as, bs) = alternate xs
    in (as, x : bs)

apply :: Direction -> Coordinates -> Coordinates
apply North (x, y) = (x, y + 1)
apply East (x, y) = (x + 1, y)
apply South (x, y) = (x, y - 1)
apply West (x, y) = (x - 1, y)
