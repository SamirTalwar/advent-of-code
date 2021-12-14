{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Bifunctor as Bifunctor
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Helpers.Function
import Helpers.Parse
import Text.Parsec

type Polymer = Map (Char, Maybe Char) Int

type Rules = Map (Char, Char) Char

main :: IO ()
main = do
  (templateString, rules) <- parseTextIO parser
  let template = Map.fromListWith (+) $ zipWith (curry (,1)) templateString (tail (map Just templateString) ++ [Nothing])
  let steps = iterate (insertBetweenPairs rules) template
  let step = steps !! 40
  let counts = List.sort $ Map.elems $ countLetters step
  let answer = last counts - head counts
  print answer

insertBetweenPairs :: Rules -> Polymer -> Polymer
insertBetweenPairs rules =
  Map.foldrWithKey accumulate Map.empty
  where
    accumulate :: (Char, Maybe Char) -> Int -> Polymer -> Polymer
    accumulate (a, Nothing) pairCount existing =
      Map.insertWith (+) (a, Nothing) pairCount existing
    accumulate (a, Just b) pairCount existing =
      let c = rules ! (a, b)
       in existing
            |> Map.insertWith (+) (a, Just c) pairCount
            |> Map.insertWith (+) (c, Just b) pairCount

countLetters :: Polymer -> Map Char Int
countLetters = Map.fromListWith (+) . map (Bifunctor.first fst) . Map.toList

parser :: Parsec Text () (String, Rules)
parser = do
  template <- many1 letter <* string "\n\n"
  rules <- Map.fromList <$> rule `endBy` string "\n"
  return (template, rules)
  where
    rule = do
      a <- letter
      b <- letter
      _ <- string " -> "
      c <- letter
      return ((a, b), c)
