{-# OPTIONS -Wall #-}

module Helpers.Numbers where

import Data.Bits ((.|.))
import qualified Data.Bits as Bits
import Data.Word (Word8)

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

word8s :: Int -> [Word8]
word8s = word8sFromWord . fromIntegral
  where
    word8sFromWord :: Word -> [Word8]
    word8sFromWord 0 = []
    word8sFromWord n = (fromIntegral n :: Word8) : word8sFromWord (Bits.shiftR n word8Size)

unWord8s :: [Word8] -> Int
unWord8s = fromIntegral . unWord8sToWord
  where
    unWord8sToWord :: [Word8] -> Word
    unWord8sToWord [] = 0
    unWord8sToWord (w : ws) = Bits.shiftL (unWord8sToWord ws) word8Size .|. fromIntegral w

word8Size :: Int
word8Size = Bits.finiteBitSize (0 :: Word8)
