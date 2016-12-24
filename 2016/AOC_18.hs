import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO

data Tile = Safe | Trap
  deriving (Eq)

instance Show Tile where
  show Safe = "."
  show Trap = "^"

rowCount = 400000

main = do
  input <- Text.strip <$> IO.getContents
  let firstRow = parseInput input
  let rows = iterate nextRow firstRow
  let safeTileCount = length $ filter (== Safe) $ concat $ take rowCount rows
  print safeTileCount

parseInput :: Text -> [Tile]
parseInput = map parseTile . Text.unpack
  where
  parseTile '.' = Safe
  parseTile '^' = Trap

nextRow :: [Tile] -> [Tile]
nextRow row = map nextTile previous
  where
  previous = windows 3 (Safe : row ++ [Safe])
  nextTile [Trap, Trap, Safe] = Trap
  nextTile [Safe, Trap, Trap] = Trap
  nextTile [Trap, Safe, Safe] = Trap
  nextTile [Safe, Safe, Trap] = Trap
  nextTile _ = Safe

windows :: Int -> [a] -> [[a]]
windows n = takeWhile (\window -> length window == n) . List.transpose . take n . List.tails
