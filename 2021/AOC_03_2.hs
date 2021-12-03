{-# OPTIONS -Wall #-}

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Parsec

data Bit = Off | On
  deriving (Eq, Show)

main :: IO ()
main = do
  report <- map parseInput . filter (not . Text.null) . Text.lines <$> IO.getContents
  let oxygenGeneratorRating = locateValue mostCommon report
  let co2ScrubberRating = locateValue leastCommon report
  let answer = binaryToInt oxygenGeneratorRating * binaryToInt co2ScrubberRating
  print answer

locateValue :: ([Bit] -> Bit) -> [[Bit]] -> [Bit]
locateValue p report =
  let firstColumn = map head report
      pValue = p firstColumn
      filtered = filter (List.isPrefixOf [pValue]) report
   in if length filtered <= 1
        then head filtered
        else pValue : locateValue p (map tail filtered)

binaryToInt :: [Bit] -> Int
binaryToInt = binaryToInt' . reverse
  where
    binaryToInt' [] = 0
    binaryToInt' (Off : xs) = binaryToInt' xs * 2
    binaryToInt' (On : xs) = binaryToInt' xs * 2 + 1

mostCommon :: [Bit] -> Bit
mostCommon xs =
  case compare offs ons of
    LT -> On
    EQ -> On
    GT -> Off
  where
    offs = length $ filter (== Off) xs
    ons = length xs - offs

leastCommon :: [Bit] -> Bit
leastCommon = flipBit . mostCommon

flipBit :: Bit -> Bit
flipBit Off = On
flipBit On = Off

parseInput :: Text -> [Bit]
parseInput = either (error . show) id . parse parser ""
  where
    parser = many1 value
    value = try (char '0' >> pure Off) <|> try (char '1' >> pure On)
