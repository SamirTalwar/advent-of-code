{-# OPTIONS -Wall #-}

module Helpers.Map where

import Data.Map (Map)
import qualified Data.Map as Map

countValues :: (Foldable t, Ord k) => t k -> Map k Int
countValues = foldr (\x -> Map.insertWith (+) x 1) Map.empty
