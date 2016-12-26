import           Data.Array
import           Data.Ix
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import           Text.Parsec
import           Text.Parsec.Text

data Command =
    Rect Int Int
  | RotateColumn Int Int
  | RotateRow Int Int
  deriving (Eq)

instance Show Command where
  show (Rect x y) = "rect " ++ show x ++ "x" ++ show y
  show (RotateColumn x amount) = "rotate column x=" ++ show x ++ " by " ++ show amount
  show (RotateRow y amount) = "rotate row y=" ++ show y ++ " by " ++ show amount

main = do
  input <- Text.lines <$> IO.getContents
  let commands = map parseCommand input
  let screen = foldl (flip applyCommand) emptyScreen commands
  print $ countPixels screen

parseCommand :: Text.Text -> Command
parseCommand text = either (error . show) id $ parse parser "" text
  where
  parser = try parseRect <|> try parseRotateColumn <|> try parseRotateRow
  parseRect = do
    string "rect "
    x <- number
    char 'x'
    y <- number
    return $ Rect x y
  parseRotateColumn = do
    string "rotate column x="
    position <- number
    string " by "
    amount <- number
    return $ RotateColumn position amount
  parseRotateRow = do
    string "rotate row y="
    position <- number
    string " by "
    amount <- number
    return $ RotateRow position amount
  number = read <$> many1 digit

screenWidth = 50
screenHeight = 6
screenBounds = ((0, 0), (screenWidth - 1, screenHeight - 1))
emptyScreen = listArray screenBounds (repeat False)

applyCommand (Rect width height) screen =
  screen // map (\i -> (i, True)) [(x, y) | y <- [0..height - 1], x <- [0..width - 1]]
applyCommand (RotateColumn x amount) screen =
  screen // map (\y -> ((x, y), screen ! (x, (y - amount) `mod` screenHeight))) [0..screenHeight - 1]
applyCommand (RotateRow y amount) screen =
  screen // map (\x -> ((x, y), screen ! ((x - amount) `mod` screenWidth, y))) [0..screenWidth - 1]

countPixels screen = length $ filter id $ map (screen !) $ range screenBounds
