{-# OPTIONS -Wall #-}

import qualified Data.List as List
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers.Grid (Grid, Point, (!))
import qualified Helpers.Grid as Grid

main :: IO ()
main = do
  heightMap <- Grid.fromDigits <$> getContents
  let lowestPoints = findLowestPoints heightMap
  let basins = findBasins heightMap lowestPoints
  let basinSizes = reverse . List.sort $ map length basins
  print $ product $ take 3 basinSizes

findLowestPoints :: Grid Int -> Set Point
findLowestPoints heightMap =
  Set.fromList
    [ points
      | points <- Grid.allPoints heightMap,
        let value = heightMap ! points
         in all (value <) (Grid.neighboringValues points heightMap)
    ]

findBasins :: Grid Int -> Set Point -> [[Point]]
findBasins heightMap lowestPoints =
  map snd $ Map.toList $ Map.delete Nothing $ Map.fromListWith (++) (map (\c -> (runToLowestPoint heightMap lowestPoints c, pure c)) (Grid.allPoints heightMap))

runToLowestPoint :: Grid Int -> Set Point -> Point -> Maybe Point
runToLowestPoint heightMap lowestPoints points
  | heightMap ! points == 9 = Nothing
  | points `Set.member` lowestPoints = Just points
  | otherwise =
    let value = heightMap ! points
     in List.find (\neighbor -> value > heightMap ! neighbor) (Grid.neighboringPoints points heightMap)
          >>= runToLowestPoint heightMap lowestPoints
