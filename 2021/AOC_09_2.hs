{-# OPTIONS -Wall #-}

import qualified Data.List as List
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers.Matrix (Matrix, (!))
import qualified Helpers.Matrix as Matrix

main :: IO ()
main = do
  heightMap <- Matrix.fromDigits <$> getContents
  let lowestPoints = findLowestPoints heightMap
  let basins = findBasins heightMap lowestPoints
  let basinSizes = reverse . List.sort $ map length basins
  print $ product $ take 3 basinSizes

findLowestPoints :: Matrix Int -> Set (Int, Int)
findLowestPoints heightMap =
  Set.fromList
    [ coordinates
      | coordinates <- Matrix.allCoordinates heightMap,
        let value = heightMap ! coordinates
         in all (value <) (Matrix.neighboringValues coordinates heightMap)
    ]

findBasins :: Matrix Int -> Set (Int, Int) -> [[(Int, Int)]]
findBasins heightMap lowestPoints =
  map snd $ Map.toList $ Map.delete Nothing $ Map.fromListWith (++) (map (\c -> (runToLowestPoint heightMap lowestPoints c, pure c)) (Matrix.allCoordinates heightMap))

runToLowestPoint :: Matrix Int -> Set (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
runToLowestPoint heightMap lowestPoints coordinates
  | heightMap ! coordinates == 9 = Nothing
  | coordinates `Set.member` lowestPoints = Just coordinates
  | otherwise =
    let value = heightMap ! coordinates
     in List.find (\neighbor -> value > heightMap ! neighbor) (Matrix.neighboringCoordinates coordinates heightMap)
          >>= runToLowestPoint heightMap lowestPoints
