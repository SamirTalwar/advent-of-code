{-# LANGUAGE ScopedTypeVariables #-}

main = do
  depths :: [Int] <- map read . filter (not . null) . lines <$> getContents
  let windows = map (\(a, b, c) -> a + b + c) $ zip3 depths (tail depths) (tail (tail depths))
  let answer = length $ filter (uncurry (<)) $ zip windows (tail windows)
  print answer
