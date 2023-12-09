main :: IO ()
main = do
  input <- map (map (read @Int) . words) . lines <$> getContents
  let nexts = map computeNext input
      result = sum nexts
  print result

computeNext :: [Int] -> Int
computeNext values =
  if all (== 0) values
    then 0
    else head values - computeNext differences
  where
    differences = map (uncurry (flip (-))) (pairs values)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [_] = []
pairs [x, y] = [(x, y)]
pairs (x : y : rest) = (x, y) : pairs (y : rest)
