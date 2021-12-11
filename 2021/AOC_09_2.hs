{-# OPTIONS -Wall #-}

import qualified Data.List as List
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers.Grid (Grid, (!))
import qualified Helpers.Grid as Grid

main :: IO ()
main = do
  heightMap <- Grid.fromDigits <$> getContents
  let lowestPoints = findLowestPoints heightMap
  let basins = findBasins heightMap lowestPoints
  let basinSizes = reverse . List.sort $ map length basins
  print $ product $ take 3 basinSizes

findLowestPoints :: Grid Int -> Set (Int, Int)
findLowestPoints heightMap =
  Set.fromList
    [ coordinates
      | coordinates <- Grid.allCoordinates heightMap,
        let value = heightMap ! coordinates
         in all (value <) (Grid.neighboringValues coordinates heightMap)
    ]

findBasins :: Grid Int -> Set (Int, Int) -> [[(Int, Int)]]
findBasins heightMap lowestPoints =
  map snd $ Map.toList $ Map.delete Nothing $ Map.fromListWith (++) (map (\c -> (runToLowestPoint heightMap lowestPoints c, pure c)) (Grid.allCoordinates heightMap))

runToLowestPoint :: Grid Int -> Set (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
runToLowestPoint heightMap lowestPoints coordinates
  | heightMap ! coordinates == 9 = Nothing
  | coordinates `Set.member` lowestPoints = Just coordinates
  | otherwise =
    let value = heightMap ! coordinates
     in List.find (\neighbor -> value > heightMap ! neighbor) (Grid.neighboringCoordinates coordinates heightMap)
          >>= runToLowestPoint heightMap lowestPoints
