{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Data.Array (Array)
import Data.Array qualified as Array
import Data.List qualified as List
import Data.Ord (Down (..))

{-# ANN module "HLint: ignore Use tuple-section" #-}

type Bid = Int

type Hand = Array Int Card

data Card = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Queen | King | Ace
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
    jokerIndices :: [Int] = map fst . filter ((== Joker) . snd) $ Array.assocs hand
    jokerReplacements = [] : mapM (\i -> map (\card -> (i, card)) allCardsExceptJoker) jokerIndices
    replaced = map (hand Array.//) jokerReplacements
    count aHand = List.sortOn Down $ filter (> 0) $ map (\card -> length (filter (== card) (Array.elems aHand))) allCards
    counts = maximum $ map count replaced
    allCards :: [Card] = [minBound .. maxBound]
    allCardsExceptJoker = tail allCards

parse :: String -> (Hand, Bid)
parse input =
  let (handInput, rest) = break (== ' ') input
   in (Array.listArray (0, 4) (parseHand handInput), read (tail rest))
  where
    parseHand :: String -> [Card]
    parseHand "" = []
    parseHand (card : rest) = parseCard card : parseHand rest
    parseCard :: Char -> Card
    parseCard = \case
      'J' -> Joker
      '2' -> Two
      '3' -> Three
      '4' -> Four
      '5' -> Five
      '6' -> Six
      '7' -> Seven
      '8' -> Eight
      '9' -> Nine
      'T' -> Ten
      'Q' -> Queen
      'K' -> King
      'A' -> Ace
      other -> error $ "Invalid card: " <> show other
