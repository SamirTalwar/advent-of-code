import qualified Data.List as List

main = do
  containerSizes <- map read <$> lines <$> getContents
  let containerCombinations = concatMap (\k -> combinations k containerSizes) [0 .. length containerSizes]
  print $ length $ filter (== 150) $ map sum containerCombinations

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n list = [x : xs | x : ts <- List.tails list, xs <- combinations (n - 1) ts]
