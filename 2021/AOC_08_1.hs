{-# OPTIONS -Wall #-}

import Data.Functor (($>))
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers.Parse
import Text.Parsec

type Input = [([Set Segment], [Set Segment])]

data Segment = A | B | C | D | E | F | G
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  input <- parseInput
  let outputValues = concatMap snd input
  let answer = sum $ map (`countOfSize` outputValues) [2, 4, 3, 7]
  print answer

countOfSize :: Int -> [Set Segment] -> Int
countOfSize n = length . filter (\segments -> Set.size segments == n)

parseInput :: IO Input
parseInput = parseLinesIO $ do
  signalPatterns <- filter (not . Set.null) <$> sepBy segments (string " ")
  _ <- string "| "
  outputValue <- sepBy segments (string " ")
  return (signalPatterns, outputValue)
  where
    segments = Set.fromList <$> many segment
    segment =
      choice
        [ char 'a' $> A,
          char 'b' $> B,
          char 'c' $> C,
          char 'd' $> D,
          char 'e' $> E,
          char 'f' $> F,
          char 'g' $> G
        ]
