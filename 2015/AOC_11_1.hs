import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Numeric

type Password = String

passwordCharacters = ['a'..'z']
passwordBase = length passwordCharacters

main = do
  password <- trim <$> getContents
  let nextPasswords = filter valid $ tail $ iterate next password
  putStrLn $ head nextPasswords

trim :: String -> String
trim = takeWhile (not . Char.isSpace) . dropWhile Char.isSpace

next :: Password -> Password
next password = Numeric.showIntAtBase passwordBase (passwordCharacters !!) (asNumber + 1) ""
  where
  [(asNumber, "")] =
    Numeric.readInt
      passwordBase
      (`elem` passwordCharacters)
      (Maybe.fromJust . (`List.elemIndex` passwordCharacters))
      password

valid password = hasIncreasingStraight && hasNoConfusingLetters && hasTwoPairs password
  where
  hasIncreasingStraight = any (`List.isInfixOf` password) $ straights
  straights = windows 3 passwordCharacters
  hasNoConfusingLetters = all (not . (`elem` confusingLetters)) password
  confusingLetters = "iol"
  hasTwoPairs [] = False
  hasTwoPairs [x] = False
  hasTwoPairs (x : xs@(y : ys))
    | x == y = hasPair ys
    | otherwise = hasTwoPairs xs
  hasPair [] = False
  hasPair [x] = False
  hasPair (x : xs@(y : _))
    | x == y = True
    | otherwise = hasPair xs

windows :: Int -> [a] -> [[a]]
windows n = takeWhile (\window -> length window == n) . List.transpose . take n . List.tails
