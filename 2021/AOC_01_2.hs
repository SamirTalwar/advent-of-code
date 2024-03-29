{-# OPTIONS -Wall #-}
{-# LANGUAGE ScopedTypeVariables #-}

main :: IO ()
main = do
  depths :: [Int] <- map read . filter (not . null) . lines <$> getContents
  let windows = map (\(a, b, c) -> a + b + c) $ zip3 depths (tail depths) (tail (tail depths))
  let answer = length $ filter id $ zipWith (<) windows (tail windows)
  print answer
