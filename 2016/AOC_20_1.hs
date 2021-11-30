import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text

data IPRange = IPRange {start :: IP, end :: IP}
  deriving (Eq, Ord, Show)

type IP = Int

main = do
  input <- Text.lines <$> IO.getContents
  let blockedIPs = canonicalise $ map parseInput input
  print $ end (head blockedIPs)

parseInput :: Text -> IPRange
parseInput text = either (error . show) id $ parse parser "" text
  where
    parser = do
      start <- number
      char '-'
      end <- number
      return $ IPRange start (end + 1)
    number = read <$> many1 digit

canonicalise = mergeOverlapping . List.sort
  where
    mergeOverlapping [] = []
    mergeOverlapping [range] = [range]
    mergeOverlapping (a@(IPRange aStart aEnd) : b@(IPRange bStart bEnd) : rest)
      | bStart <= aEnd = mergeOverlapping (IPRange aStart (max aEnd bEnd) : rest)
      | otherwise = a : mergeOverlapping (b : rest)
