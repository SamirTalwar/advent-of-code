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
  let paths = findPaths connections
  print $ length paths

findPaths :: Graph Cave -> [[Cave]]
findPaths connections = findPaths' Start Set.empty
  where
    findPaths' :: Cave -> Set Cave -> [[Cave]]
    findPaths' End _ = [[End]]
    findPaths' current forbidden =
      let next = Set.toList (Set.difference (connections ! current) forbidden)
          newForbidden = case current of
            Big _ -> forbidden
            cave -> Set.insert cave forbidden
       in map (current :) (concatMap (`findPaths'` newForbidden) next)

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
