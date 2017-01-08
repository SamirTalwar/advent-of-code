import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import           Data.Set (Set)
import qualified Data.Set as Set

compartments = 3

main = do
  weights <- map read <$> lines <$> getContents
  let distribution = head $ distribute compartments weights
  let passengerCompartment = head $ List.sortBy (Ord.comparing length) distribution
  let quantumEntanglement = product passengerCompartment
  print quantumEntanglement

distribute :: Int -> [Int] -> [[Set Int]]
distribute binCount weights = map (map snd) $ distribute' (List.sortBy (flip compare) weights) emptyBins
  where
  capacity = sum weights `div` binCount
  emptyBins = replicate binCount (capacity, Set.empty)
  distribute' :: [Int] -> [(Int, Set Int)] -> [[(Int, Set Int)]]
  distribute' [] bins = [bins]
  distribute' (w : ws) bins = do
    (before, current, after) <- each bins
    inserted <- Maybe.maybeToList $ insert w current
    distribute' ws (before ++ (inserted : after))
    where
    insert weight (space, items)
      | weight <= space = Just (space - weight, Set.insert weight items)
      | otherwise = Nothing

each :: [a] -> [([a], a, [a])]
each xs = map pickOut [0..(length xs - 1)]
  where
  pickOut i =
    let (before, current : after) = splitAt i xs
    in (before, current, after)
