{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Comonad
import Data.Bool (bool)
import Helpers.Parse
import Text.Parsec

data Bit = O | X
  deriving (Eq, Ord)

type Bits = [Bit]

instance Show Bit where
  show O = "0"
  show X = "1"

data Versioned a = Versioned Int a
  deriving (Eq, Show, Functor)

instance Comonad Versioned where
  extract (Versioned _ value) = value
  duplicate versioned@(Versioned version _) = Versioned version versioned

data Packet m where
  Literal :: Int -> Packet m
  Sum :: [m (Packet m)] -> Packet m
  Product :: [m (Packet m)] -> Packet m
  Minimum :: [m (Packet m)] -> Packet m
  Maximum :: [m (Packet m)] -> Packet m
  GreaterThan :: m (Packet m) -> m (Packet m) -> Packet m
  LessThan :: m (Packet m) -> m (Packet m) -> Packet m
  EqualTo :: m (Packet m) -> m (Packet m) -> Packet m

deriving instance (Comonad m, (forall a. Show a => Show (m a))) => Show (Packet m)

main :: IO ()
main = do
  input <- parseTextIO (hexToBin <$> many1 hexDigit)
  let packet = decode input
  print $ eval packet

eval :: Comonad m => m (Packet m) -> Int
eval = eval' . extract
  where
    eval' :: Comonad m => Packet m -> Int
    eval' (Literal n) = n
    eval' (Sum packets) = sum $ map eval packets
    eval' (Product packets) = product $ map eval packets
    eval' (Minimum packets) = minimum $ map eval packets
    eval' (Maximum packets) = maximum $ map eval packets
    eval' (GreaterThan a b) = boolToInt $ eval a > eval b
    eval' (LessThan a b) = boolToInt $ eval a < eval b
    eval' (EqualTo a b) = boolToInt $ eval a == eval b
    boolToInt = bool 0 1

decode :: Bits -> Versioned (Packet Versioned)
decode = either (error . show) id . parse (versioned packet <* ending) ""
  where
    ending = many zero *> eof
    versioned :: Parsec Bits () a -> Parsec Bits () (Versioned a)
    versioned parser = Versioned <$> readBits 3 <*> parser
    packet :: Parsec Bits () (Packet Versioned)
    packet = do
      typeId <- readBits 3
      case typeId of
        4 -> literal
        0 -> Sum <$> subPackets
        1 -> Product <$> subPackets
        2 -> Minimum <$> subPackets
        3 -> Maximum <$> subPackets
        5 -> uncurry GreaterThan <$> (exactlyTwo =<< subPackets)
        6 -> uncurry LessThan <$> (exactlyTwo =<< subPackets)
        7 -> uncurry EqualTo <$> (exactlyTwo =<< subPackets)
        _ -> fail $ "Unknown type ID: " ++ show typeId
    subPackets = containing (versioned packet)
    literal :: Parsec Bits () (Packet m)
    literal = Literal . bitsToInt <$> literal'
    literal' = do
      (b : bs) <- countBits 5
      case b of
        O -> return bs
        X -> (bs <>) <$> literal'
    containing :: Parsec Bits () a -> Parsec Bits () [a]
    containing parser = do
      lengthTypeId <- bit
      case lengthTypeId of
        O -> readBits 15 >>= upTo parser
        X -> readBits 11 >>= (`count` parser)
    readBits n = bitsToInt <$> countBits n
    countBits n = count n bit
    zero = bitToken (\case O -> Just O; X -> Nothing)
    bit = bitToken Just
    bitToken = tokenPrim show (\pos _ _ -> incSourceColumn pos 1)

upTo :: Parsec s () a -> Int -> Parsec s () [a]
upTo parser n
  | n < 0 = fail "Read too far."
  | n == 0 = return []
  | otherwise = do
    start <- sourceColumn <$> getPosition
    result <- parser
    end <- sourceColumn <$> getPosition
    let remaining = n - (end - start)
    (result :) <$> upTo parser remaining

exactlyTwo :: (MonadFail m, Show a) => [a] -> m (a, a)
exactlyTwo [x, y] = return (x, y)
exactlyTwo values = fail $ "Expected exactly two values, but got: " ++ show values

hexToBin :: String -> Bits
hexToBin "" = []
hexToBin (x : xs) = hexCharToBin x <> hexToBin xs

hexCharToBin :: Char -> Bits
hexCharToBin '0' = [O, O, O, O]
hexCharToBin '1' = [O, O, O, X]
hexCharToBin '2' = [O, O, X, O]
hexCharToBin '3' = [O, O, X, X]
hexCharToBin '4' = [O, X, O, O]
hexCharToBin '5' = [O, X, O, X]
hexCharToBin '6' = [O, X, X, O]
hexCharToBin '7' = [O, X, X, X]
hexCharToBin '8' = [X, O, O, O]
hexCharToBin '9' = [X, O, O, X]
hexCharToBin 'A' = [X, O, X, O]
hexCharToBin 'B' = [X, O, X, X]
hexCharToBin 'C' = [X, X, O, O]
hexCharToBin 'D' = [X, X, O, X]
hexCharToBin 'E' = [X, X, X, O]
hexCharToBin 'F' = [X, X, X, X]
hexCharToBin c = error $ "Invalid hex character: '" ++ pure c ++ "'"

bitsToInt :: Bits -> Int
bitsToInt = bitsToInt' . reverse
  where
    bitsToInt' [] = 0
    bitsToInt' (b : bs) = bitToInt b + 2 * bitsToInt' bs
    bitToInt O = 0
    bitToInt X = 1
