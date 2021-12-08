{-# OPTIONS -Wall #-}

import Data.Functor (($>))
import qualified Data.List as List
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers.Numbers
import Helpers.Parse
import Text.Parsec

type Item = ([Set Segment], [Set Segment])

type Input = [Item]

data Segment = A | B | C | D | E | F | G
  deriving (Eq, Ord, Enum, Bounded, Show)

allSegments :: [Segment]
allSegments = [minBound .. maxBound]

numbers :: [Set Segment]
numbers =
  map
    Set.fromList
    [ [A, B, C, E, F, G],
      [C, F],
      [A, C, D, E, G],
      [A, C, D, F, G],
      [B, C, D, F],
      [A, B, D, F, G],
      [A, B, D, E, F, G],
      [A, C, F],
      [A, B, C, D, E, F, G],
      [A, B, C, D, F, G]
    ]

numbersSet :: Set (Set Segment)
numbersSet = Set.fromList numbers

main :: IO ()
main = do
  input <- parseInput
  let outputNumbers = map solve input
  print $ sum outputNumbers

solve :: Item -> Int
solve (signalPatterns, outputValue) =
  let mapping = findMapping signalPatterns
      outputPatterns = map (applyMapping mapping) outputValue
      outputDigits = map (Maybe.fromJust . (`List.elemIndex` numbers)) outputPatterns
   in digitsToIntegral outputDigits

findMapping :: [Set Segment] -> Map Segment Segment
findMapping signalPatterns =
  let possibleMappings = map (Map.fromList . zip allSegments) (List.permutations allSegments)
      mappedNumbers = List.find (\mapping -> Set.fromList (map (applyMapping mapping) signalPatterns) == numbersSet) possibleMappings
   in Maybe.fromJust mappedNumbers

applyMapping :: Map Segment Segment -> Set Segment -> Set Segment
applyMapping mapping = Set.map (mapping !)

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
