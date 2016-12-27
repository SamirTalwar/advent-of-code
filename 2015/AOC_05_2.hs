import qualified Data.List as List

main = do
  input <- lines <$> getContents
  let nice = filter isNice input
  print $ length nice

isNice :: String -> Bool
isNice string = containsPairOfPairs string && containsSandwichLetter string
  where
  containsPairOfPairs [] = False
  containsPairOfPairs [x] = False
  containsPairOfPairs (a : b : xs) =
    (a : b : "") `List.isInfixOf` xs
      || containsPairOfPairs (b : xs)
  containsSandwichLetter [] = False
  containsSandwichLetter [x] = False
  containsSandwichLetter [x, y] = False
  containsSandwichLetter (a : xs@(b : c : _)) =
    a == c || containsSandwichLetter xs
