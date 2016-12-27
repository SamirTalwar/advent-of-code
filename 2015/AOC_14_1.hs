{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import           Text.Parsec
import           Text.Parsec.Text

data Reindeer = Reindeer String Speed Time Time
  deriving (Eq, Show)
newtype Speed = KmPerSecond Int
  deriving (Eq, Show)
newtype Distance = Km Int
  deriving (Eq, Ord, Num, Show)
newtype Time = Seconds Int
  deriving (Eq, Ord, Num, Show)

raceTime :: Time
raceTime = Seconds 2503

main = do
  reindeers <- map parseInput <$> Text.lines <$> IO.getContents
  let distances = map (distance raceTime) reindeers
  print $ maximum distances

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
    return $ Seconds seconds
  number = read <$> many1 digit

distance :: Time -> Reindeer -> Distance
distance (Seconds raceTime) (Reindeer _ (KmPerSecond speed) (Seconds flightTime) (Seconds restTime)) = fly raceTime
  where
  fly time
    | time - flightTime > 0 = Km (speed * flightTime) + rest (time - flightTime)
    | otherwise = Km (speed * time)
  rest time
    | time - restTime > 0 = fly (time - restTime)
    | otherwise = Km 0
