{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad (mapM_)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import           Text.Parsec
import           Text.Parsec.Text

data CompressedText =
    Uncompressed Text
  | Compressed Text Int
  deriving (Eq)

instance Show CompressedText where
  show (Uncompressed text) = Text.unpack text
  show (Compressed text repetitions) = "(" ++ show (Text.length text) ++ "x" ++ show repetitions ++ ")" ++ Text.unpack text

main = do
  input <- Text.strip <$> IO.getContents
  let parsedInput = parseInput input
  let decompressedInput = Text.concat $ map decompress parsedInput
  print $ Text.length decompressedInput

parseInput :: Text -> [CompressedText]
parseInput text = either (error . show) id $ parse parser "" text
  where
  parser = many $ try uncompressed <|> compressed
  uncompressed = Uncompressed <$> Text.pack <$> many1 alphaNum
  compressed = do
    char '('
    segmentLength :: Int <- read <$> many1 digit
    char 'x'
    repetitions :: Int <- read <$> many1 digit
    char ')'
    compressedText <- Text.pack <$> count segmentLength anyChar
    return $ Compressed compressedText repetitions

decompress (Uncompressed text) = text
decompress (Compressed text repetitions) = Text.replicate repetitions text
