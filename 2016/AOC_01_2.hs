import qualified Data.Char as Char
import qualified Data.Set as Set

data Movement = Forward | RotateLeft | RotateRight
  deriving (Eq, Show)

data Direction = North | East | South | West
  deriving (Eq, Show)

main = do
  contents <- getContents
  let movements = decode contents
  let locations = moveLots (0, 0, North) [(0, 0)] movements
  let destination = firstVisitedTwice locations
  let blocks = distance destination
  putStrLn $ show blocks

decode "" = []
decode ('L' : rest) = RotateLeft : decodeForward rest 0
decode ('R' : rest) = RotateRight : decodeForward rest 0
decode (' ' : rest) = decode rest

decodeForward "" n = replicate n Forward
decodeForward (char : rest) n
  | Char.isDigit char = decodeForward rest (n * 10 + Char.digitToInt char)
  | otherwise = replicate n Forward ++ decode rest

moveLots position locations [] = locations
moveLots position locations (movement : rest) =
  let (newPosition, newLocations) = move movement position
   in moveLots newPosition (locations ++ newLocations) rest

move RotateLeft (x, y, North) = ((x, y, West), [])
move RotateLeft (x, y, West) = ((x, y, South), [])
move RotateLeft (x, y, South) = ((x, y, East), [])
move RotateLeft (x, y, East) = ((x, y, North), [])
move RotateRight (x, y, North) = ((x, y, East), [])
move RotateRight (x, y, East) = ((x, y, South), [])
move RotateRight (x, y, South) = ((x, y, West), [])
move RotateRight (x, y, West) = ((x, y, North), [])
move Forward (x, y, North) = ((x, y + 1, North), [(x, y + 1)])
move Forward (x, y, East) = ((x + 1, y, East), [(x + 1, y)])
move Forward (x, y, South) = ((x, y - 1, South), [(x, y - 1)])
move Forward (x, y, West) = ((x - 1, y, West), [(x - 1, y)])

firstVisitedTwice locations = firstVisitedTwice' locations Set.empty
  where
    firstVisitedTwice' (location : rest) visited
      | location `Set.member` visited = location
      | otherwise = firstVisitedTwice' rest (Set.insert location visited)

distance (x, y) = abs x + abs y
