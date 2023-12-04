import Data.Set (Set)
import Data.Set qualified as Set
import Helpers.Parse

data Card = Card {cardWinningNumbers :: Set Int, cardNumbersYouHave :: Set Int}
  deriving (Eq, Show)

main :: IO ()
main = do
  cards <- parseInput parser
  let winners = filter (not . Set.null) $ map (\(Card w h) -> w `Set.intersection` h) cards
  let points = map (\intersection -> 2 `pow` (Set.size intersection - 1)) winners
  let result = sum points
  print result

pow :: Int -> Int -> Int
pow _ 0 = 1
pow n m = 2 * pow n (pred m)

parser :: Parser [Card]
parser = many $ do
  _ <- string "Card"
  spaces
  _ <- some digitChar
  _ <- string ":"
  spaces
  winningNumbers <- Set.fromList <$> decimal `sepEndBy` spaces
  _ <- string "|"
  spaces
  numbersYouHave <- Set.fromList <$> decimal `sepEndBy` spaces
  _ <- newline
  pure $ Card winningNumbers numbersYouHave
