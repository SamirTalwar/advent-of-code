{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Monad (mapM_)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import           Text.Parsec

data IP = IPv7 [String] [String]
  deriving (Eq)

instance Show IP where
  show (IPv7 outers inners) = head outers ++ concat (zipWith (\i o -> "[" ++ i ++ "]" ++ o) inners (tail outers))

main = do
  ips :: [IP] <- map parseIP <$> lines <$> getContents
  print $ length $ filter supportsSSL ips

parseIP :: String -> IP
parseIP text = case parse parser "" text of
  Right ip -> ip
  where
  parser = do
    first <- many1 letter
    (inners, outers) <- unzip <$> (many1 $ do
      char '['
      inner <- many1 letter
      char ']'
      outer <- many1 letter
      return (inner, outer))
    return $ IPv7 (first : outers) inners

supportsTLS :: IP -> Bool
supportsTLS (IPv7 outers inners) =
  any hasABBA outers && not (any hasABBA inners)

hasABBA :: String -> Bool
hasABBA = any isABBA . windows 4

isABBA :: String -> Bool
isABBA [a, b, c, d] = a /= b && c /= d && a == d && b == c

supportsSSL :: IP -> Bool
supportsSSL (IPv7 outers inners) =
  any (\aba -> any (hasBAB aba) inners) $ concatMap getABAs outers

getABAs :: String -> [String]
getABAs = filter isABA . windows 3

hasABA :: String -> Bool
hasABA = any isABA . windows 3

hasBAB :: String -> String -> Bool
hasBAB aba = any (== bab) . windows 3
  where
  [a, b, _] = aba
  bab = [b, a, b]

isABA :: String -> Bool
isABA [a, b, c] = a /= b && a == c

windows :: Int -> [a] -> [[a]]
windows n = takeWhile (\window -> length window == n) . List.transpose . take n . List.tails
