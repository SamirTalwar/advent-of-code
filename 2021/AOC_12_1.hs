{-# OPTIONS -Wall #-}

import qualified Data.Bifunctor as Bifunctor
import Data.Functor (($>))
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Tuple as Tuple
import Helpers.Parse
import Text.Parsec

data Cave = Start | End | Big String | Small String
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  connectionList <- parseLinesIO parser
  let connections = createConnectionMap connectionList
  let paths = findPaths connections
  print $ length paths

createConnectionMap :: [(Cave, Cave)] -> Map Cave (Set Cave)
createConnectionMap connections = Map.fromListWith (<>) (map (Bifunctor.second Set.singleton) (connections ++ map Tuple.swap connections))

findPaths :: Map Cave (Set Cave) -> [[Cave]]
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
