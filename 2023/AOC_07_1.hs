{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}

import Data.List qualified as List
import Data.Ord (Down (..))

type Bid = Int

type Hand = [Card]

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving stock (Eq, Ord, Enum, Bounded, Show)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind
  deriving stock (Eq, Ord, Show)

main :: IO ()
main = do
  input <- map parse . lines <$> getContents
  let typed = map (\(hand, bid) -> ((handType hand, hand), bid)) input
      ranked = List.sortOn fst typed
      bids = zipWith (*) [1 ..] $ map snd ranked
      result = sum bids
  print result

handType :: Hand -> HandType
handType hand =
  case counts of
    [5] -> FiveOfAKind
    [4, 1] -> FourOfAKind
    [3, 2] -> FullHouse
    [3, 1, 1] -> ThreeOfAKind
    [2, 2, 1] -> TwoPair
    [2, 1, 1, 1] -> OnePair
    [1, 1, 1, 1, 1] -> HighCard
    _ -> error $ "Unknown hand type for hand: " <> show hand
  where
    counts = List.sortOn Down $ filter (> 0) $ map (\card -> length (filter (== card) hand)) [minBound .. maxBound]

parse :: String -> (Hand, Bid)
parse input =
  let (handInput, rest) = break (== ' ') input
   in (parseHand handInput, read (tail rest))
  where
    parseHand :: String -> [Card]
    parseHand "" = []
    parseHand (card : rest) = parseCard card : parseHand rest
    parseCard :: Char -> Card
    parseCard = \case
      '2' -> Two
      '3' -> Three
      '4' -> Four
      '5' -> Five
      '6' -> Six
      '7' -> Seven
      '8' -> Eight
      '9' -> Nine
      'T' -> Ten
      'J' -> Jack
      'Q' -> Queen
      'K' -> King
      'A' -> Ace
      other -> error $ "Invalid card: " <> show other
