main :: IO ()
main = do
  [raceTime, raceRecord] <- map parse . lines <$> getContents
  let result = wins raceTime raceRecord
  print result

-- solve for: x * (time - x) - record
-- -x^2 + time * x - record
-- x = (time ± sqrt(time^2 - 4 * record)) / 2
-- we want the range between the two solutions, inclusive
wins :: Int -> Int -> Int
wins time record =
  let a = floor $ (fromIntegral time + middle) / 2
      b = ceiling $ (fromIntegral time - middle) / 2
   in abs (a - b) + 1
  where
    middle :: Double = sqrt . fromIntegral $ time * time - 4 * record

parse :: String -> Int
parse input = read . concat . chunk $ dropWhile (/= ' ') input
  where
    chunk :: String -> [String]
    chunk "" = []
    chunk input' =
      let (value, rest) = span (/= ' ') $ dropWhile (== ' ') input'
       in value : chunk rest
