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
  let answer = countPaths connections
  print answer

countPaths :: Graph Cave -> Int
countPaths connections = countPaths' Set.empty False Start
  where
    countPaths' :: Set Cave -> Bool -> Cave -> Int
    countPaths' _ _ End = 1
    countPaths' forbidden visitedASmallCaveTwice current =
      let potentialConnections = connections ! current
          allowedConnections = Set.toList (Set.difference potentialConnections forbidden)
          forbiddenConnections = filter isSmall (Set.toList (Set.intersection potentialConnections forbidden))
          newForbidden = case current of
            Big _ -> forbidden
            cave -> Set.insert cave forbidden
          next =
            if visitedASmallCaveTwice
              then map (,True) allowedConnections
              else map (,False) allowedConnections ++ map (,True) forbiddenConnections
          counts = map (\(nextCave, newVisitedASmallCaveTwice) -> countPaths' newForbidden newVisitedASmallCaveTwice nextCave) next
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

isSmall :: Cave -> Bool
isSmall (Small _) = True
isSmall _ = False
