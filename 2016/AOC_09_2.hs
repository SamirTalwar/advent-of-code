{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (mapM_)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text

data CompressedText
  = Uncompressed Int
  | Compressed Text Int
  deriving (Eq)

main = do
  input <- Compressed <$> (Text.strip <$> IO.getContents) <*> return 1
  print $ measureLength input

measureLength :: CompressedText -> Int
measureLength (Uncompressed length) = length
measureLength compressed@(Compressed text repetitions) = repetitions * sum (map measureLength (parseCompressed text))

parseCompressed :: Text -> [CompressedText]
parseCompressed text = either (error . show) id $ parse parser "" text
  where
    parser = many $ try uncompressed <|> compressed
    uncompressed = Uncompressed <$> length <$> many1 alphaNum
    compressed = do
      char '('
      segmentLength :: Int <- read <$> many1 digit
      char 'x'
      repetitions :: Int <- read <$> many1 digit
      char ')'
      compressedText <- Text.pack <$> count segmentLength anyChar
      return $ Compressed compressedText repetitions
