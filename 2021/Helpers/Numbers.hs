{-# OPTIONS -Wall #-}

module Helpers.Numbers where

triangular :: Int -> Int
triangular n = n * succ n `div` 2
