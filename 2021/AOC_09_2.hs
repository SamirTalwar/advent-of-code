{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.List as List
import qualified Data.Map.Lazy as Map
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  heightMap :: [[Int]] <- map (map (read . (: []))) . lines <$> getContents
  let lowestPoints = findLowestPoints heightMap
  let basins = findBasins heightMap lowestPoints
  let basinSizes = reverse . List.sort $ map length basins
  print $ product $ take 3 basinSizes

findLowestPoints :: [[Int]] -> Set (Int, Int)
findLowestPoints heightMap =
  let width = length (head heightMap)
      height = length heightMap
   in Set.fromList
        [ (x, y)
          | x <- [0 .. width - 1],
            y <- [0 .. height - 1],
            let value = heightMap !! y !! x
             in all (\(neighborX, neighborY) -> value < heightMap !! neighborY !! neighborX) (neighbors x y width height)
        ]

findBasins :: [[Int]] -> Set (Int, Int) -> [[(Int, Int)]]
findBasins heightMap lowestPoints =
  let width = length (head heightMap)
      height = length heightMap
      allCoordinates = [(x, y) | x <- [0 .. width - 1], y <- [0 .. height - 1]]
   in map snd $ Map.toList $ Map.delete Nothing $ Map.fromListWith (++) (map (\c -> (runToLowestPoint heightMap width height lowestPoints c, pure c)) allCoordinates)

runToLowestPoint :: [[Int]] -> Int -> Int -> Set (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
runToLowestPoint heightMap width height lowestPoints p@(x, y)
  | heightMap !! y !! x == 9 = Nothing
  | p `Set.member` lowestPoints = Just p
  | otherwise = do
    newP <- List.find (\(neighborX, neighborY) -> heightMap !! y !! x > heightMap !! neighborY !! neighborX) (neighbors x y width height)
    runToLowestPoint heightMap width height lowestPoints newP

neighbors :: Int -> Int -> Int -> Int -> [(Int, Int)]
neighbors x y width height =
  filter (uncurry (inBounds width height)) [(x, y - 1), (x - 1, y), (x, y + 1), (x + 1, y)]

inBounds :: Int -> Int -> Int -> Int -> Bool
inBounds width height x y = x >= 0 && x < width && y >= 0 && y < height
