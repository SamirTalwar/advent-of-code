import qualified Data.List as List

compartments = 4

main = do
  weights <- map read <$> lines <$> getContents
  let capacity = sum weights `div` compartments
  let compartments = filter ((== capacity) . sum) $ distribute weights
  let passengerCompartmentPackageCount = minimum $ map length compartments
  let passengerCompartments = filter (\pc -> length pc == passengerCompartmentPackageCount) compartments
  let quantumEntanglements = map product passengerCompartments
  let quantumEntanglement = minimum quantumEntanglements
  print quantumEntanglement

distribute :: [Int] -> [[Int]]
distribute weights = concatMap (\i -> combinations i weights) [0..(length weights `div` compartments)]
  where
  sortedWeights = List.sortBy (flip compare) weights

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n list = [x : xs | x : ts <- List.tails list, xs <- combinations (n - 1) ts]
