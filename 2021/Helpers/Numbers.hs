{-# OPTIONS -Wall #-}

module Helpers.Numbers where

import qualified Data.Bits as Bits

digitsToIntegral :: (Integral a) => [a] -> a
digitsToIntegral = digitsToIntegral' . reverse
  where
    digitsToIntegral' [] = error "No digits."
    digitsToIntegral' [x] = x
    digitsToIntegral' (x : xs) = x + digitsToIntegral' xs * 10

triangular :: Int -> Int
triangular n = n * succ n `div` 2

bits :: Int -> [Bool]
bits 0 = []
bits n = map (Bits.testBit n) [0 .. Bits.finiteBitSize (0 :: Int) - 1]

unBits :: [Bool] -> Int
unBits = foldr (\(i, b) n -> if b then Bits.setBit n i else n) 0 . zip [0 ..]
