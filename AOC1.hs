import qualified Data.Char as Char

data Movement = Forward | RotateLeft | RotateRight
  deriving (Eq, Show)

data Direction = North | East | South | West
  deriving (Eq, Show)

main = do
  contents <- getContents
  let movements = decode contents
  let destination = foldr move (0, 0, North) movements
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

move RotateLeft (x, y, North) = (x, y, West)
move RotateLeft (x, y, West) = (x, y, South)
move RotateLeft (x, y, South) = (x, y, East)
move RotateLeft (x, y, East) = (x, y, North)
move RotateRight (x, y, North) = (x, y, East)
move RotateRight (x, y, East) = (x, y, South)
move RotateRight (x, y, South) = (x, y, West)
move RotateRight (x, y, West) = (x, y, North)
move Forward (x, y, North) = (x, y + 1, North)
move Forward (x, y, East) = (x + 1, y, East)
move Forward (x, y, South) = (x, y - 1, South)
move Forward (x, y, West) = (x - 1, y, West)

distance (x, y, _) = abs x + abs y
