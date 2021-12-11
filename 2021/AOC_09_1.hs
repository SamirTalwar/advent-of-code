{-# OPTIONS -Wall #-}

import Helpers.Grid (Grid, (!))
import qualified Helpers.Grid as Grid

main :: IO ()
main = do
  heightMap <- Grid.fromDigits <$> getContents
  let lowestPoints = findLowestPoints heightMap
  print $ sum $ map succ lowestPoints

findLowestPoints :: Grid Int -> [Int]
findLowestPoints heightMap =
  [ heightMap ! coordinates
    | coordinates <- Grid.allCoordinates heightMap,
      let value = heightMap ! coordinates
       in all (value <) (Grid.neighboringValues coordinates heightMap)
  ]
