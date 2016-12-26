import qualified Data.Char as Char
import qualified Data.List as List

data Direction = Up | Down

main = do
  directions <- parseInput <$> trim <$> getContents
  let result = List.elemIndex (-1) $ scanl (flip apply) 0 directions
  print result

trim :: String -> String
trim = takeWhile (not . Char.isSpace) . dropWhile Char.isSpace

parseInput :: String -> [Direction]
parseInput = map parseChar
  where
  parseChar '(' = Up
  parseChar ')' = Down

apply :: Direction -> Int -> Int
apply Up n = n + 1
apply Down n = n - 1
