{-# OPTIONS -Wall #-}
{-# LANGUAGE LambdaCase #-}

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

data Packet = Literal Int | SubPackets [Versioned Packet]
  deriving (Eq, Show)

main :: IO ()
main = do
  input <- parseTextIO (hexToBin <$> many1 hexDigit)
  let packet = decode input
  print $ sumVersions packet

sumVersions :: Versioned Packet -> Int
sumVersions (Versioned v (Literal _)) = v
sumVersions (Versioned v (SubPackets packets)) = v + sum (map sumVersions packets)

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
        _ -> operator
      return $ Versioned version value
    literal :: Parsec Bits () Packet
    literal = Literal . bitsToInt <$> literal'
    literal' = do
      (b : bs) <- countBits 5
      case b of
        O -> return bs
        X -> (bs <>) <$> literal'
    operator :: Parsec Bits () Packet
    operator = do
      lengthTypeId <- bit
      case lengthTypeId of
        O -> SubPackets <$> (readBits 15 >>= packetsUpTo)
        X -> readBits 11 >>= (\n -> SubPackets <$> count n packet)
    packetsUpTo :: Int -> Parsec Bits () [Versioned Packet]
    packetsUpTo 0 = return []
    packetsUpTo n
      | n < 0 = fail "Read too far."
      | otherwise = do
        start <- sourceColumn <$> getPosition
        p <- packet
        end <- sourceColumn <$> getPosition
        ps <- packetsUpTo (n - (end - start))
        return (p : ps)
    readBits n = bitsToInt <$> countBits n
    countBits n = count n bit
    zero = tokenPrim show (\pos _ _ -> incSourceColumn pos 1) (\case O -> Just O; X -> Nothing)
    bit = tokenPrim show (\pos _ _ -> incSourceColumn pos 1) Just

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
