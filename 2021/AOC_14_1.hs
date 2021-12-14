{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import Helpers.Graph (Graph)
import qualified Helpers.Graph as Graph
import Helpers.Parse
import Text.Parsec

type Polymer = String

main :: IO ()
main = do
  (template, rules) <- parseTextIO parser
  let steps = iterate (insertBetweenPairs rules) template
  let step = steps !! 10
  let counts :: [Int] = List.sort $ Map.elems $ Map.fromListWith (+) $ map (,1) step
  let answer = last counts - head counts
  print answer

insertBetweenPairs :: Graph Polymer -> Polymer -> Polymer
insertBetweenPairs rules template =
  let pairs = zip template (tail template)
   in head template : concatMap (\(a, b) -> Maybe.fromJust (Graph.lookupOnly [a, b] rules) ++ [b]) pairs

parser :: Parsec Text () (Polymer, Graph Polymer)
parser = do
  template <- polymer <* string "\n\n"
  rules <- Graph.directedGraph <$> rule `endBy` string "\n"
  return (template, rules)
  where
    polymer = many1 letter
    rule = do
      start <- polymer
      _ <- string " -> "
      end <- polymer
      return (start, end)
