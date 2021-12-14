{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections #-}

module Helpers.Graph
  ( Graph,
    directedGraph,
    undirectedGraph,
    lookup,
    lookupOnly,
    (!),
    toList,
  )
where

import qualified Data.Bifunctor as Bifunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import Prelude hiding (lookup)

newtype Graph a = Graph (Map a (Set a))

directedGraph :: Ord a => [(a, a)] -> Graph a
directedGraph connectionList =
  Graph $ Map.fromListWith (<>) $ map (Bifunctor.second Set.singleton) connectionList

undirectedGraph :: Ord a => [(a, a)] -> Graph a
undirectedGraph connectionList =
  directedGraph $ connectionList ++ map Tuple.swap connectionList

lookup :: Ord a => a -> Graph a -> Set a
lookup from (Graph connections) = Maybe.fromMaybe Set.empty $ Map.lookup from connections

lookupOnly :: Ord a => a -> Graph a -> Maybe a
lookupOnly from graph
  | null to = Nothing
  | Set.size to == 1 = Maybe.listToMaybe $ Set.toList to
  | otherwise = error "Too many items in the graph."
  where
    to = lookup from graph

(!) :: Ord a => Graph a -> a -> Set a
(!) graph from = lookup from graph

toList :: Graph a -> [(a, a)]
toList (Graph connections) = do
  (from, to) <- Map.toList connections
  map (from,) (Set.toList to)
