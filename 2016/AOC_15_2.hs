import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text

data Position = Position Int Int
  deriving (Eq, Show)

main = do
  input <- Text.lines <$> IO.getContents
  let positions = map parseInput input
  print $ solve positions

parseInput :: Text -> Position
parseInput text = either (error . show) id $ parse parser "" text
  where
    parser = do
      string "Disc #"
      number
      string " has "
      count <- number
      string " positions; at time=0, it is at position "
      start <- number
      string "."
      return $ Position count start
    number = read <$> many1 digit

solve :: [Position] -> Maybe (Int, [Position])
solve positions = do
  List.find (valid . snd) $ zip [0 ..] $ iterate (map step) positions

step :: Position -> Position
step (Position count current) = Position count ((current + 1) `mod` count)

valid :: [Position] -> Bool
valid positions = all (\(Position _ c) -> c == 0) $ map (\(t, p) -> iterate step p !! t) $ zip [1 ..] positions
