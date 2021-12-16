{-# OPTIONS -Wall #-}
{-# LANGUAGE LambdaCase #-}

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
  deriving (Eq, Show)

data Packet
  = Literal Int
  | Sum [Versioned Packet]
  | Product [Versioned Packet]
  | Minimum [Versioned Packet]
  | Maximum [Versioned Packet]
  | GreaterThan (Versioned Packet) (Versioned Packet)
  | LessThan (Versioned Packet) (Versioned Packet)
  | EqualTo (Versioned Packet) (Versioned Packet)
  deriving (Eq, Show)

main :: IO ()
main = do
  input <- parseTextIO (hexToBin <$> many1 hexDigit)
  let packet = decode input
  print $ eval packet

eval :: Versioned Packet -> Int
eval (Versioned _ (Literal n)) = n
eval (Versioned _ (Sum packets)) = sum $ map eval packets
eval (Versioned _ (Product packets)) = product $ map eval packets
eval (Versioned _ (Minimum packets)) = minimum $ map eval packets
eval (Versioned _ (Maximum packets)) = maximum $ map eval packets
eval (Versioned _ (GreaterThan a b)) = bool 0 1 $ eval a > eval b
eval (Versioned _ (LessThan a b)) = bool 0 1 $ eval a < eval b
eval (Versioned _ (EqualTo a b)) = bool 0 1 $ eval a == eval b

decode :: Bits -> Versioned Packet
decode = either (error . show) id . parse (packet <* ending) ""
  where
    ending = many zero *> eof
    packet :: Parsec Bits () (Versioned Packet)
    packet = do
      version <- readBits 3
      typeId <- readBits 3
      value <- case typeId of
        4 -> literal
        0 -> Sum <$> subPackets
        1 -> Product <$> subPackets
        2 -> Minimum <$> subPackets
        3 -> Maximum <$> subPackets
        5 -> uncurry GreaterThan <$> (exactlyTwo =<< subPackets)
        6 -> uncurry LessThan <$> (exactlyTwo =<< subPackets)
        7 -> uncurry EqualTo <$> (exactlyTwo =<< subPackets)
        _ -> fail $ "Unknown type ID: " ++ show typeId
      return $ Versioned version value
    literal :: Parsec Bits () Packet
    literal = Literal . bitsToInt <$> literal'
    literal' = do
      (b : bs) <- countBits 5
      case b of
        O -> return bs
        X -> (bs <>) <$> literal'
    subPackets :: Parsec Bits () [Versioned Packet]
    subPackets = do
      lengthTypeId <- bit
      case lengthTypeId of
        O -> readBits 15 >>= upTo packet
        X -> readBits 11 >>= (`count` packet)
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
