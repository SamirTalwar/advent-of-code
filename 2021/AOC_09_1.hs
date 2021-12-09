{-# OPTIONS -Wall #-}

import Helpers.Matrix (Matrix, (!))
import qualified Helpers.Matrix as Matrix

main :: IO ()
main = do
  heightMap <- Matrix.fromList . map (map (read . (: []))) . lines <$> getContents
  let lowestPoints = findLowestPoints heightMap
  print $ sum $ map succ lowestPoints

findLowestPoints :: Matrix Int -> [Int]
findLowestPoints heightMap =
  [ heightMap ! coordinates
    | coordinates <- Matrix.allCoordinates heightMap,
      let value = heightMap ! coordinates
       in all (value <) (Matrix.neighboringValues coordinates heightMap)
  ]
