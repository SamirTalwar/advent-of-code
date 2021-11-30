import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text

data Size = BoxSize Int Int Int
  deriving (Eq, Show)

main = do
  sizes <- map parseInput <$> Text.lines <$> IO.getContents
  let result = sum $ map paperRequired sizes
  print result

parseInput :: Text -> Size
parseInput text = either (error . show) id $ parse parser "" text
  where
    parser = do
      a <- number
      char 'x'
      b <- number
      char 'x'
      c <- number
      let [a', b', c'] = List.sort [a, b, c]
      return $ BoxSize a' b' c'
    number = read <$> many1 digit

paperRequired :: Size -> Int
paperRequired (BoxSize a b c) = 3 * a * b + 2 * a * c + 2 * b * c
