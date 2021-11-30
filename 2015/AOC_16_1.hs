import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text

data Sue = Sue Int (Set Fact)
  deriving (Eq, Show)

data Fact = Fact String Int
  deriving (Eq, Ord, Show)

factsAboutSue =
  Set.fromList
    [ Fact "children" 3,
      Fact "cats" 7,
      Fact "samoyeds" 2,
      Fact "pomeranians" 3,
      Fact "akitas" 0,
      Fact "vizslas" 0,
      Fact "goldfish" 5,
      Fact "trees" 3,
      Fact "cars" 2,
      Fact "perfumes" 1
    ]

main = do
  sues <- map parseInput <$> Text.lines <$> IO.getContents
  let candidates = filter (\(Sue _ facts) -> facts `Set.isSubsetOf` factsAboutSue) sues
  mapM_ print candidates

parseInput :: Text -> Sue
parseInput text = either (error . show) id $ parse parser "" text
  where
    parser = do
      string "Sue "
      n <- number
      string ": "
      facts <- Set.fromList <$> fact `sepBy1` string ", "
      return $ Sue n facts
    fact = do
      item <- many1 letter
      string ": "
      count <- number
      return $ Fact item count
    number = read <$> (many1 digit <|> ((:) <$> char '-' <*> many1 digit))
