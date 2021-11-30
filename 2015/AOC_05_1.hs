main = do
  input <- lines <$> getContents
  let nice = filter isNice input
  print $ length nice

isNice :: String -> Bool
isNice string = hasThreeVowels && containsDoubleLetter && doesNotContainNaughtyTuplets
  where
    hasThreeVowels = length (filter (`elem` vowels) string) >= 3
    containsDoubleLetter = any (uncurry (==)) $ stringInPairs
    doesNotContainNaughtyTuplets = not $ any (`elem` naughtyTuplets) $ stringInPairs
    stringInPairs = pairs string
    vowels = "aeiou"
    naughtyTuplets = [('a', 'b'), ('c', 'd'), ('p', 'q'), ('x', 'y')]

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [x] = []
pairs (a : xs@(b : _)) = (a, b) : pairs xs
