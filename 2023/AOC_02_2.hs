import Data.Functor (($>))
import Data.List qualified as List
import Data.Maybe (mapMaybe)
import Helpers.Parse

newtype Game = Game [Reveal]
  deriving (Eq, Show)

data Reveal = Reveal
  { revealRed :: Maybe Int,
    revealGreen :: Maybe Int,
    revealBlue :: Maybe Int
  }
  deriving (Eq, Show)

data Color = Red | Green | Blue
  deriving (Eq, Show)

main :: IO ()
main = do
  games <- parseInput parser
  let powers = map gamePower games
  let result = sum powers
  print result

gamePower :: Game -> Int
gamePower (Game reveals) = red * green * blue
  where
    red = maximum $ mapMaybe revealRed reveals
    green = maximum $ mapMaybe revealGreen reveals
    blue = maximum $ mapMaybe revealBlue reveals

parser :: Parser [Game]
parser = many $ do
  _ <- string "Game"
  spaces
  _ <- some digitChar
  _ <- string ":"
  spaces
  reveals <- reveal `sepBy` (char ';' *> spaces)
  _ <- newline
  pure $ Game reveals
  where
    reveal :: Parser Reveal
    reveal = do
      colors <- (flip (,) <$> decimal <* spaces <*> color) `sepBy` (char ',' *> spaces)
      let red = List.lookup Red colors
          green = List.lookup Green colors
          blue = List.lookup Blue colors
      pure $ Reveal red green blue
    color :: Parser Color
    color =
      choice
        [ string "red" $> Red,
          string "green" $> Green,
          string "blue" $> Blue
        ]
