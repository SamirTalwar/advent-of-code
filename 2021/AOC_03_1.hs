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
  let columns = List.transpose report
  let gamma = map mostCommon columns
  let epsilon = map flipBit gamma
  let answer = binaryToInt gamma * binaryToInt epsilon
  print answer

binaryToInt :: [Bit] -> Int
binaryToInt = unBits . map toBool . reverse

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

toBool :: Bit -> Bool
toBool Off = False
toBool On = True

parseInput :: IO [[Bit]]
parseInput = parseLinesIO $ many1 $ try (char '0' >> pure Off) <|> try (char '1' >> pure On)
