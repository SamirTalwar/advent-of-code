{-# OPTIONS -Wall #-}

module Helpers.Graph
  ( Graph,
    undirectedGraph,
    (!),
  )
where

import qualified Data.Bifunctor as Bifunctor
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple

newtype Graph a = Graph (Map a (Set a))

undirectedGraph :: Ord a => [(a, a)] -> Graph a
undirectedGraph connectionList =
  Graph $ Map.fromListWith (<>) (map (Bifunctor.second Set.singleton) (connectionList ++ map Tuple.swap connectionList))

(!) :: Ord a => Graph a -> a -> Set a
(!) (Graph connections) from = connections Map.! from
