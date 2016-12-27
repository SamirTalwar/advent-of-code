import qualified Data.Char as Char
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import qualified Numeric
import           Text.Parsec
import           Text.Parsec.Text

main = do
  stringLiterals <- Text.lines <$> IO.getContents
  let strings = map parseInput stringLiterals
  let sizeOfStringLiterals = sum $ map Text.length stringLiterals
  let sizeOfStrings = sum $ map Text.length strings
  print sizeOfStringLiterals
  print sizeOfStrings
  print $ sizeOfStringLiterals - sizeOfStrings

parseInput :: Text -> Text
parseInput text = either (error . show) id $ parse parser "" text
  where
  parser = do
    char '"'
    s <- Text.pack <$> many character
    char '"'
    return s
  character = escaped <|> unescaped
  escaped = char '\\' >> (try backslash <|> try doubleQuote <|> hex)
  backslash = char '\\'
  doubleQuote = char '"'
  hex = do
    char 'x'
    hexDigits <- count 2 hexDigit
    return $ Char.chr $ fst $ head $ Numeric.readHex hexDigits
  unescaped = letter <|> digit
