import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import           Text.Parsec
import           Text.Parsec.Text

data Demand = Demand { demandRow :: Int, demandColumn :: Int }
  deriving (Show)

start = 20151125
multiplier = 252533
modulus = 33554393

main = do
  input <- IO.getContents
  let demand = parseInput input
  let iterations = codeNumber demand
  let result = iterate computeNext start !! (iterations - 1)
  print result

parseInput :: Text -> Demand
parseInput text = either (error . show) id $ parse parser "" text
  where
  parser = do
    string "To continue, please consult the code grid in the manual.  Enter the code at row "
    row <- number
    string ", column "
    column <- number
    string "."
    return $ Demand row column
  number = read <$> many1 digit

codeNumber :: Demand -> Int
codeNumber (Demand row column) = n * (n + 1) `div` 2 - row + 1
  where
  n = row + column - 1

computeNext :: Int -> Int
computeNext value = (value * multiplier) `mod` modulus
