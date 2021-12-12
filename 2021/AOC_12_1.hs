{-# OPTIONS -Wall #-}

import Data.Functor (($>))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Helpers.Graph (Graph, (!))
import qualified Helpers.Graph as Graph
import Helpers.Parse
import Text.Parsec

data Cave = Start | End | Big String | Small String
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  connectionList <- parseLinesIO parser
  let connections = Graph.undirectedGraph connectionList
  let answer = countPaths connections
  print answer

countPaths :: Graph Cave -> Int
countPaths connections = countPaths' Set.empty Start
  where
    countPaths' :: Set Cave -> Cave -> Int
    countPaths' _ End = 1
    countPaths' forbidden current =
      let newForbidden = case current of
            Big _ -> forbidden
            cave -> Set.insert cave forbidden
          next = Set.toList (Set.difference (connections ! current) forbidden)
          counts = map (countPaths' newForbidden) next
       in sum counts

parser :: Parsec Text () (Cave, Cave)
parser = do
  caveA <- cave
  _ <- string "-"
  caveB <- cave
  return (caveA, caveB)
  where
    cave =
      choice $
        map
          try
          [ string "start" $> Start,
            string "end" $> End,
            Big <$> many1 upper,
            Small <$> many1 lower
          ]
