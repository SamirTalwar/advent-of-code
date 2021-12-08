{-# OPTIONS -Wall #-}

module Helpers.Numbers where

digitsToIntegral :: (Integral a) => [a] -> a
digitsToIntegral = digitsToIntegral' . reverse
  where
    digitsToIntegral' [] = error "No digits."
    digitsToIntegral' [x] = x
    digitsToIntegral' (x : xs) = x + digitsToIntegral' xs * 10

triangular :: Int -> Int
triangular n = n * succ n `div` 2
