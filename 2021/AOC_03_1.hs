{-# OPTIONS -Wall #-}

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Helpers.Parse
import Text.Parsec

data Bit = Off | On
  deriving (Eq, Show)

main :: IO ()
main = do
  report <- map parseInput . filter (not . Text.null) . Text.lines <$> IO.getContents
  let columns = List.transpose report
  let gamma = map mostCommon columns
  let epsilon = map flipBit gamma
  let answer = binaryToInt gamma * binaryToInt epsilon
  print answer

binaryToInt :: [Bit] -> Int
binaryToInt = binaryToInt' . reverse
  where
    binaryToInt' [] = 0
    binaryToInt' (Off : xs) = binaryToInt' xs * 2
    binaryToInt' (On : xs) = binaryToInt' xs * 2 + 1

mostCommon :: [Bit] -> Bit
mostCommon xs =
  case compare fs ts of
    LT -> On
    EQ -> error "No most common value."
    GT -> Off
  where
    fs = length $ filter (== Off) xs
    ts = length $ filter (== On) xs

flipBit :: Bit -> Bit
flipBit Off = On
flipBit On = Off

parseInput :: Text -> [Bit]
parseInput = parseText $ many1 $ try (char '0' >> pure Off) <|> try (char '1' >> pure On)
