main :: IO ()
main = do
  [raceTime, raceRecord] <- map parse . lines <$> getContents
  let result = wins raceTime raceRecord
  print result

wins :: Int -> Int -> Int
wins time record =
  let lossTimes = takeWhile (<= record) $ map (\charge -> charge * (time - charge)) [0 .. time]
      losses = length lossTimes * 2
   in time - losses + 1

parse :: String -> Int
parse input = read . concat . chunk $ dropWhile (/= ' ') input
  where
    chunk :: String -> [String]
    chunk "" = []
    chunk input' =
      let (value, rest) = span (/= ' ') $ dropWhile (== ' ') input'
       in value : chunk rest
