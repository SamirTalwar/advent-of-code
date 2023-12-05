import Data.Array.MArray
import Data.Array.ST
import Data.Foldable (forM_)
import Data.Set (Set)
import Data.Set qualified as Set
import Helpers.Parse

data Card = Card {cardWinningNumbers :: Set Int, cardNumbersYouHave :: Set Int}
  deriving (Eq, Show)

main :: IO ()
main = do
  cards <- parseInput parser
  let winners = zip [1 ..] $ map (\(Card w h) -> Set.size (w `Set.intersection` h)) cards
  let results = runSTArray $ do
        owned <- newArray (1, length cards) (1 :: Int)
        forM_ winners $ \(cardNo, winning) -> do
          held <- readArray owned cardNo
          forM_ [cardNo + 1 .. cardNo + winning] $ \cardToUpdate -> do
            currentValue <- readArray owned cardToUpdate
            writeArray owned cardToUpdate (currentValue + held)
        pure owned
  let result = sum results
  print result

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
