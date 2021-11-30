import qualified Data.List as List

main = do
  containerSizes <- map read <$> lines <$> getContents
  let containerCombinations = map (\k -> combinations k containerSizes) [0 .. length containerSizes]
  print $ head $ dropWhile (== 0) $ map (\combinations -> length $ filter (== 150) $ map sum combinations) containerCombinations

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n list = [x : xs | x : ts <- List.tails list, xs <- combinations (n - 1) ts]
