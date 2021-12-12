{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections #-}

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
findPaths connections = findPaths' Start Set.empty False
  where
    findPaths' :: Cave -> Set Cave -> Bool -> [[Cave]]
    findPaths' End _ _ = [[End]]
    findPaths' current forbidden visitedASmallCaveTwice = do
      let potentialConnections = connections ! current
      let allowedConnections = Set.toList (Set.difference potentialConnections forbidden)
      let forbiddenConnections = filter isSmall (Set.toList (Set.intersection potentialConnections forbidden))
      let newForbidden = case current of
            Big _ -> forbidden
            cave -> Set.insert cave forbidden
      (nextCave, newVisitedASmallCaveTwice) <-
        if visitedASmallCaveTwice
          then map (,True) allowedConnections
          else map (,False) allowedConnections ++ map (,True) forbiddenConnections
      path <- findPaths' nextCave newForbidden newVisitedASmallCaveTwice
      return (current : path)

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

isSmall :: Cave -> Bool
isSmall (Small _) = True
isSmall _ = False
