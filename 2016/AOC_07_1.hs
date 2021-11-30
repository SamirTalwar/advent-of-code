{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad (mapM_)
import qualified Data.List as List
import Text.Parsec

data IP = IPv7 [String] [String]
  deriving (Eq)

instance Show IP where
  show (IPv7 outers inners) = head outers ++ concat (zipWith (\i o -> "[" ++ i ++ "]" ++ o) inners (tail outers))

main = do
  ips :: [IP] <- map parseIP <$> lines <$> getContents
  print $ length $ filter supportsTLS ips

parseIP :: String -> IP
parseIP text = case parse parser "" text of
  Right ip -> ip
  where
    parser = do
      first <- many1 letter
      (inners, outers) <-
        unzip
          <$> ( many1 $ do
                  char '['
                  inner <- many1 letter
                  char ']'
                  outer <- many1 letter
                  return (inner, outer)
              )
      return $ IPv7 (first : outers) inners

supportsTLS :: IP -> Bool
supportsTLS (IPv7 outers inners) =
  any hasABBA outers && not (any hasABBA inners)

hasABBA :: String -> Bool
hasABBA = any isABBA . takeWhile (\window -> length window == 4) . List.transpose . take 4 . List.tails
  where
    isABBA :: String -> Bool
    isABBA [a, b, c, d] = a /= b && c /= d && a == d && b == c
