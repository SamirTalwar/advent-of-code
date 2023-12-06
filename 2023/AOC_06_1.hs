main :: IO ()
main = do
  [raceTimes, raceRecords] <- map values . lines <$> getContents
  let result = product $ zipWith wins raceTimes raceRecords
  print result

wins :: Int -> Int -> Int
wins time record =
  let lossTimes = takeWhile (<= record) $ map (\charge -> charge * (time - charge)) [0 .. time]
      losses = length lossTimes * 2
   in time - losses + 1

values :: String -> [Int]
values input = map read . chunk $ dropWhile (/= ' ') input
  where
    chunk :: String -> [String]
    chunk "" = []
    chunk input' =
      let (value, rest) = span (/= ' ') $ dropWhile (== ' ') input'
       in value : chunk rest
