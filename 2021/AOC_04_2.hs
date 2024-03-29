{-# OPTIONS -Wall #-}

import qualified Data.List as List
import qualified Data.List.Split as Split
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Helpers.Applicative
import Helpers.Parse
import Text.Parsec

type Card = [[Maybe Int]]

main :: IO ()
main = do
  input <- Text.lines <$> IO.getContents
  let draws = parseDraws $ head input
  let cards = map parseCards . filter (not . null) . Split.splitWhen Text.null $ tail input
  let (finalDraw, winningCard) = run draws cards
  let unmarked = sum $ map (Maybe.fromMaybe 0) (concat winningCard)
  print $ finalDraw * unmarked

run :: [Int] -> [Card] -> (Int, Card)
run [] _ = error "No more draws."
run (draw : rest) cards =
  let newCards = map (applyDraw draw) cards
      (winners, losers) = List.partition isWinner newCards
   in case (winners, length losers) of
        (w : _, 0) -> (draw, w)
        (_, _) -> run rest losers

applyDraw :: Int -> Card -> Card
applyDraw number = map (map (\x -> if x == Just number then Nothing else x))

isWinner :: Card -> Bool
isWinner = isWinnerHorizontal <||> isWinnerVertical
  where
    isWinnerHorizontal = any (all Maybe.isNothing)
    isWinnerVertical = isWinnerHorizontal . List.transpose

parseDraws :: Text -> [Int]
parseDraws = parseText $ sepBy1 int (string ",")

parseCards :: [Text] -> Card
parseCards = map (parseText (optional spaces >> sepBy1 (Just <$> int) spaces))
