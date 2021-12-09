{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

main :: IO ()
main = do
  heightMap :: [[Int]] <- map (map (read . (: []))) . lines <$> getContents
  let lowestPoints = findLowestPoints heightMap
  print $ sum $ map succ lowestPoints

findLowestPoints :: [[Int]] -> [Int]
findLowestPoints heightMap =
  let width = length (head heightMap)
      height = length heightMap
   in [ heightMap !! y !! x
        | x <- [0 .. width - 1],
          y <- [0 .. height - 1],
          let value = heightMap !! y !! x
           in all (\(neighborX, neighborY) -> value < heightMap !! neighborY !! neighborX) (neighbors x y width height)
      ]

neighbors :: Int -> Int -> Int -> Int -> [(Int, Int)]
neighbors x y width height =
  filter (uncurry (inBounds width height)) [(x, y - 1), (x - 1, y), (x, y + 1), (x + 1, y)]

inBounds :: Int -> Int -> Int -> Int -> Bool
inBounds width height x y = x >= 0 && x < width && y >= 0 && y < height
