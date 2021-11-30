import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text

data Sue = Sue Int [Fact]
  deriving (Eq, Show)

data Fact = Fact String Int
  deriving (Eq, Show)

factsAboutSue =
  Map.fromList
    [ ("children", (== 3)),
      ("cats", (> 7)),
      ("samoyeds", (== 2)),
      ("pomeranians", (< 3)),
      ("akitas", (== 0)),
      ("vizslas", (== 0)),
      ("goldfish", (< 5)),
      ("trees", (> 3)),
      ("cars", (== 2)),
      ("perfumes", (== 1))
    ]

main = do
  sues <- map parseInput <$> Text.lines <$> IO.getContents
  let candidates = filter (\(Sue _ facts) -> check facts) sues
  mapM_ print candidates

parseInput :: Text -> Sue
parseInput text = either (error . show) id $ parse parser "" text
  where
    parser = do
      string "Sue "
      n <- number
      string ": "
      facts <- fact `sepBy1` string ", "
      return $ Sue n facts
    fact = do
      item <- many1 letter
      string ": "
      count <- number
      return $ Fact item count
    number = read <$> (many1 digit <|> ((:) <$> char '-' <*> many1 digit))

check :: [Fact] -> Bool
check facts = all checkFact facts
  where
    checkFact (Fact item count) =
      case Map.lookup item factsAboutSue of
        Nothing -> True
        Just checkCount -> checkCount count
