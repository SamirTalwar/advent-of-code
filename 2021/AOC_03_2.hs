{-# OPTIONS -Wall #-}

import qualified Data.List as List
import Helpers.Numbers
import Helpers.Parse
import Text.Parsec

data Bit = Off | On
  deriving (Eq, Show)

main :: IO ()
main = do
  report <- parseInput
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
binaryToInt = unBits . map toBool . reverse

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

toBool :: Bit -> Bool
toBool Off = False
toBool On = True

parseInput :: IO [[Bit]]
parseInput = parseLinesIO $ many1 $ try (char '0' >> pure Off) <|> try (char '1' >> pure On)
