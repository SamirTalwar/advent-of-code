{-# OPTIONS -Wall #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Helpers.Numbers where

import Data.Bits (Bits, (.|.))
import qualified Data.Bits as Bits
import Data.Int
import Data.Kind (Type)
import Data.Word

class (Integral (Unsigned from), Bits (Unsigned from)) => ToUnsigned from where
  type Unsigned from :: Type
  toUnsigned :: from -> Unsigned from
  fromUnsigned :: Unsigned from -> from

instance ToUnsigned Word where
  type Unsigned Word = Word
  toUnsigned = id
  fromUnsigned = id

instance ToUnsigned Word8 where
  type Unsigned Word8 = Word8
  toUnsigned = id
  fromUnsigned = id

instance ToUnsigned Word16 where
  type Unsigned Word16 = Word16
  toUnsigned = id
  fromUnsigned = id

instance ToUnsigned Word32 where
  type Unsigned Word32 = Word32
  toUnsigned = id
  fromUnsigned = id

instance ToUnsigned Word64 where
  type Unsigned Word64 = Word64
  toUnsigned = id
  fromUnsigned = id

instance ToUnsigned Int where
  type Unsigned Int = Word
  toUnsigned = fromIntegral
  fromUnsigned = fromIntegral

instance ToUnsigned Int8 where
  type Unsigned Int8 = Word8
  toUnsigned = fromIntegral
  fromUnsigned = fromIntegral

instance ToUnsigned Int16 where
  type Unsigned Int16 = Word16
  toUnsigned = fromIntegral
  fromUnsigned = fromIntegral

instance ToUnsigned Int32 where
  type Unsigned Int32 = Word32
  toUnsigned = fromIntegral
  fromUnsigned = fromIntegral

instance ToUnsigned Int64 where
  type Unsigned Int64 = Word64
  toUnsigned = fromIntegral
  fromUnsigned = fromIntegral

-- type family ToUnsigned (from :: Type) :: Type where
--   ToUnsigned Word = Word
--   ToUnsigned Word8 = Word8
--   ToUnsigned Word16 = Word16
--   ToUnsigned Word32 = Word32
--   ToUnsigned Word64 = Word64
--   ToUnsigned Int = Word
--   ToUnsigned Int8 = Word8
--   ToUnsigned Int16 = Word16
--   ToUnsigned Int32 = Word32
--   ToUnsigned Int64 = Word64

digitsToIntegral :: (Integral a) => [a] -> a
digitsToIntegral = digitsToIntegral' . reverse
  where
    digitsToIntegral' [] = error "No digits."
    digitsToIntegral' [x] = x
    digitsToIntegral' (x : xs) = x + digitsToIntegral' xs * 10

triangular :: Int -> Int
triangular n = n * succ n `div` 2

bits :: (Num a, Bits a) => a -> [Bool]
bits 0 = []
bits n = map (Bits.testBit n) [0 .. Bits.finiteBitSize (0 :: Int) - 1]

unBits :: (Num a, Bits a) => [Bool] -> a
unBits = foldr (\(i, b) n -> if b then Bits.setBit n i else n) 0 . zip [0 ..]

word8s :: forall a. ToUnsigned a => a -> [Word8]
word8s = word8sUnsigned . toUnsigned
  where
    word8sUnsigned :: (Integral a, Bits a) => a -> [Word8]
    word8sUnsigned 0 = []
    word8sUnsigned m = (fromIntegral m :: Word8) : word8sUnsigned (Bits.shiftR m word8Size)

unWord8s :: forall a. ToUnsigned a => [Word8] -> a
unWord8s = fromUnsigned . unWord8sUnsigned
  where
    unWord8sUnsigned :: (Integral a, Bits a) => [Word8] -> a
    unWord8sUnsigned [] = 0
    unWord8sUnsigned (w : ws) = Bits.shiftL (unWord8sUnsigned ws) word8Size .|. fromIntegral w

word8Size :: Int
word8Size = Bits.finiteBitSize (0 :: Word8)
