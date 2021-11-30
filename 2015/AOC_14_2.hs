{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text

data Reindeer = Reindeer String Speed Time Time
  deriving (Eq, Show)

data Status = Status {reindeer :: Reindeer, distance :: Distance, points :: Points}
  deriving (Eq, Show)

newtype Speed = KmPerSecond Int
  deriving (Eq, Show)

newtype Distance = Km Int
  deriving (Eq, Ord, Num, Show)

type Time = Int

type Points = Int

raceTime :: Time
raceTime = 2503

main = do
  reindeers <- map parseInput <$> Text.lines <$> IO.getContents
  let statuses = map (\reindeer -> Status reindeer (Km 0) 0) reindeers
  let results = List.foldl' (flip progress) statuses [0 .. (raceTime - 1)]
  print $ maximum $ map points results

parseInput :: Text -> Reindeer
parseInput text = either (error . show) id $ parse parser "" text
  where
    parser = do
      name <- many1 letter
      string " can fly "
      sp <- speed
      string " for "
      flightTime <- time
      string ", but then must rest for "
      restTime <- time
      string "."
      return $ Reindeer name sp flightTime restTime
    speed = do
      magnitude <- number
      string " km/s"
      return $ KmPerSecond magnitude
    time = do
      seconds <- number
      string " seconds"
      return $ seconds
    number = read <$> many1 digit

progress :: Time -> [Status] -> [Status]
progress currentTime = assignPoints . map (proceed currentTime)

assignPoints :: [Status] -> [Status]
assignPoints statuses = map assignPointsForFurthestDistance statuses
  where
    assignPointsForFurthestDistance status@(Status _ d p)
      | d == furthestDistance = status {points = p + 1}
      | otherwise = status
    furthestDistance = maximum $ map distance statuses

proceed :: Time -> Status -> Status
proceed currentTime status@(Status (Reindeer _ (KmPerSecond speed) flightTime restTime) distance _)
  | isFlying currentTime flightTime restTime = status {distance = distance + Km speed}
  | otherwise = status

isFlying :: Time -> Time -> Time -> Bool
isFlying currentTime flightTime restTime =
  currentTime `mod` (flightTime + restTime) < flightTime
