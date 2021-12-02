{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

main :: IO ()
main = do
  depths :: [Int] <- map read . filter (not . null) . lines <$> getContents
  let answer = length $ filter id $ zipWith (<) depths (tail depths)
  print answer
