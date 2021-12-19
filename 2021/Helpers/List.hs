{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections #-}

module Helpers.List where

import qualified Data.List as List

selectPairs :: [a] -> [(a, a)]
selectPairs xs = concat $ zipWith (\i x -> let (before, after) = List.splitAt i xs in map (x,) (before ++ tail after)) [0 ..] xs
