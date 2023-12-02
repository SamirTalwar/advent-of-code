import Data.Bifunctor qualified as Bifunctor
import Data.List qualified as List

main :: IO ()
main = do
  inputLines <- lines <$> getContents
  let numbers :: [Int] = map (read . (\xs -> [parseForwards xs, parseBackwards xs])) inputLines
  let result = sum numbers
  print result

parseForwards :: String -> Char
parseForwards = parseFirst forwardPatterns

parseBackwards :: String -> Char
parseBackwards = parseFirst backwardsPatterns . reverse

parseFirst :: [(String, Char)] -> String -> Char
parseFirst _ "" = error "No pattern found."
parseFirst patterns input =
  maybe (parseFirst patterns (tail input)) snd $
    List.find ((`List.isPrefixOf` input) . fst) patterns

forwardPatterns :: [(String, Char)]
forwardPatterns =
  [ ("0", '0'),
    ("1", '1'),
    ("2", '2'),
    ("3", '3'),
    ("4", '4'),
    ("5", '5'),
    ("6", '6'),
    ("7", '7'),
    ("8", '8'),
    ("9", '9'),
    ("one", '1'),
    ("two", '2'),
    ("three", '3'),
    ("four", '4'),
    ("five", '5'),
    ("six", '6'),
    ("seven", '7'),
    ("eight", '8'),
    ("nine", '9')
  ]

backwardsPatterns :: [(String, Char)]
backwardsPatterns = map (Bifunctor.first reverse) forwardPatterns
