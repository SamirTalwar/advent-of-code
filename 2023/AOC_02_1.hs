import Data.Functor (void, ($>))
import Data.List qualified as List
import Data.Void (Void)
import Text.Megaparsec as Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Megaparsec.Parsec Void String

data Game = Game {gameId :: Int, gameReveals :: [Reveal]}
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
  contents <- getContents
  games <- either (fail . Megaparsec.errorBundlePretty) pure $ Megaparsec.parse parser "input" contents
  let possibleGames = filter possibleGame games
  let result = sum $ map gameId possibleGames
  print result

possibleGame :: Game -> Bool
possibleGame (Game _ reveals) = all possibleReveal reveals

possibleReveal :: Reveal -> Bool
possibleReveal (Reveal red green blue) = all (<= 12) red && all (<= 13) green && all (<= 14) blue

parser :: Parser [Game]
parser = many game <* eof
  where
    game :: Parser Game
    game = do
      _ <- string "Game"
      spaces
      gameId <- integer
      _ <- string ":"
      spaces
      reveals <- reveal `sepBy` (char ';' *> spaces)
      _ <- newline
      pure $ Game gameId reveals
    reveal :: Parser Reveal
    reveal = do
      colors <- (flip (,) <$> integer <* spaces <*> color) `sepBy` (char ',' *> spaces)
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
    integer :: (Integral a) => Parser a
    integer = Lexer.decimal
    spaces :: Parser ()
    spaces = void $ some (char ' ')
