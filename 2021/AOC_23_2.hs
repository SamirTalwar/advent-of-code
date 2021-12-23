{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Data.Foldable (toList)
import Data.Functor
import qualified Data.Maybe as Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Helpers.Function
import Helpers.Memoization
import Helpers.Parse
import Text.Parsec

depth :: Int
depth = 4

data Amphipod = A | B | C | D
  deriving (Eq, Ord, Show)

instance HasTrie Amphipod where
  data Amphipod :->: x = AmphipodTrie x x x x
  trie f = AmphipodTrie (f A) (f B) (f C) (f D)
  unTrie (AmphipodTrie a _ _ _) A = a
  unTrie (AmphipodTrie _ b _ _) B = b
  unTrie (AmphipodTrie _ _ c _) C = c
  unTrie (AmphipodTrie _ _ _ d) D = d

data Room = Room Amphipod [Amphipod]
  deriving (Eq, Ord, Show)

instance HasTrie Room where
  newtype Room :->: x = RoomTrie ((Amphipod, [Amphipod]) :->: x)
  trie f = RoomTrie $ trie $ \(expected, actual) -> f (Room expected actual)
  unTrie (RoomTrie f) (Room expected actual) = curry (unTrie f) expected actual

data Burrow = Burrow (Seq Room) (Seq (Maybe Amphipod))
  deriving (Eq, Ord)

instance HasTrie Burrow where
  newtype Burrow :->: x = BurrowTrie ((Seq Room, Seq (Maybe Amphipod)) :->: x)
  trie f = BurrowTrie $ trie $ \(rooms, hallway) -> f (Burrow rooms hallway)
  unTrie (BurrowTrie f) (Burrow rooms hallway) = curry (unTrie f) rooms hallway

instance Show Burrow where
  show (Burrow rooms hallway) =
    "Burrow: "
      ++ unwords (map (\(Room _ actual) -> show actual) (toList rooms))
      ++ " "
      ++ concatMap (\case Nothing -> "."; Just x -> show x) (toList hallway)

type Queue a = Set (QueueEntry a)

data QueueEntry a = QueueEntry {_queueEntryCost :: Int, queueEntryValue :: a}
  deriving (Eq, Ord, Functor)

main :: IO ()
main = do
  burrow <- parseTextIO parser
  let answers = organize (Set.singleton (QueueEntry 0 burrow))
  print $ head answers

organize :: Queue Burrow -> [Int]
organize queue =
  case Set.minView queue of
    Nothing -> []
    Just (QueueEntry cost burrow@(Burrow rooms _), rest)
      | all (\(Room expected actual) -> length actual == depth && all (== expected) actual) rooms -> cost : organize rest
      | otherwise -> organize (Set.fromList (memoMove burrow |> map (addCost cost)) `Set.union` rest)

memoMove :: Burrow -> [QueueEntry Burrow]
memoMove = memo move

move :: Burrow -> [QueueEntry Burrow]
move burrow@(Burrow rooms hallway) =
  ( fmap (uncurry (moveFromHallway burrow)) ((\i -> maybe Seq.empty (\x -> Seq.singleton (i, x)) (hallway ! i)) =<< ints (Seq.length hallway))
      <> Seq.zipWith (moveFromRoom burrow) (ints (Seq.length rooms)) rooms
  )
    |> concat

moveFromRoom :: Burrow -> Int -> Room -> [QueueEntry Burrow]
moveFromRoom (Burrow rooms hallway) roomNumber (Room expected room)
  | all (== expected) room = []
  | otherwise =
    let (amphipod : room') = room
        position = roomNumber * 2 + 2
        (allLeft, allRight) = Seq.splitAt position $ Seq.zip (ints (Seq.length hallway)) hallway
        left = (\(newPosition, _) -> QueueEntry (costOf amphipod (position - newPosition + (depth - length room) + 1)) newPosition) <$> Seq.takeWhileR (Maybe.isNothing . snd) allLeft
        right = (\(newPosition, _) -> QueueEntry (costOf amphipod (newPosition - position + (depth - length room) + 1)) newPosition) <$> Seq.takeWhileL (Maybe.isNothing . snd) allRight
        freeHallway = left <> right
        validHallway = Seq.filter ((`Set.member` hallwayDestinations) . queueEntryValue) freeHallway

        destination = destinationRoomNumber amphipod
        Room destinationExpected destinationRoom = rooms ! destination
        roomDirection = if destination <= roomNumber then left else right
        updatedDestinationRoom =
          if length destinationRoom < depth && all (== destinationExpected) destinationRoom
            then
              (const (Room destinationExpected (amphipod : destinationRoom)) <$>)
                . addCost (costOf amphipod (depth - length destinationRoom))
                . (roomDirection !)
                <$> Seq.findIndexL ((== destination * 2 + 2) . queueEntryValue) roomDirection
            else Nothing

        toHallwayResult = fmap (\newPosition -> Burrow (Seq.update roomNumber (Room expected room') rooms) (Seq.update newPosition (Just amphipod) hallway)) <$> toList validHallway
        toRoomResult = Maybe.maybeToList $ fmap (\newRoom -> Burrow (Seq.update destination newRoom (Seq.update roomNumber (Room expected room') rooms)) hallway) <$> updatedDestinationRoom
     in toHallwayResult ++ toRoomResult

moveFromHallway :: Burrow -> Int -> Amphipod -> [QueueEntry Burrow]
moveFromHallway (Burrow rooms hallway) position amphipod =
  let (allLeft, allRight) = Seq.splitAt position $ Seq.zip (ints (Seq.length hallway)) hallway
      left = (\(newPosition, _) -> QueueEntry (costOf amphipod (position - newPosition)) newPosition) <$> Seq.takeWhileR (Maybe.isNothing . snd) allLeft
      right = (\(newPosition, _) -> QueueEntry (costOf amphipod (newPosition - position)) newPosition) <$> Seq.takeWhileL (Maybe.isNothing . snd) (Seq.drop 1 allRight)
      destination = destinationRoomNumber amphipod
      Room destinationExpected destinationRoom = rooms ! destination
      roomDirection = if destination <= (position - 2) `div` 2 then left else right
      updatedDestinationRoom =
        if length destinationRoom < depth && all (== destinationExpected) destinationRoom
          then
            (const (Room destinationExpected (amphipod : destinationRoom)) <$>)
              . addCost (costOf amphipod (depth - length destinationRoom))
              . (roomDirection !)
              <$> Seq.findIndexL ((== destination * 2 + 2) . queueEntryValue) roomDirection
          else Nothing
   in Maybe.maybeToList $ fmap (\newRoom -> Burrow (Seq.update destination newRoom rooms) (Seq.update position Nothing hallway)) <$> updatedDestinationRoom

hallwayDestinations :: Set Int
hallwayDestinations = Set.fromList [0, 1, 3, 5, 7, 9, 10]

destinationRoomNumber :: Amphipod -> Int
destinationRoomNumber A = 0
destinationRoomNumber B = 1
destinationRoomNumber C = 2
destinationRoomNumber D = 3

addCost :: Int -> QueueEntry a -> QueueEntry a
addCost increase (QueueEntry cost value) = QueueEntry (cost + increase) value

costOf :: Amphipod -> Int -> Int
costOf amphipod distance = moveEnergy amphipod * distance

moveEnergy :: Amphipod -> Int
moveEnergy A = 1
moveEnergy B = 10
moveEnergy C = 100
moveEnergy D = 1000

ints :: Int -> Seq Int
ints n = Seq.iterateN n succ 0

(!) :: Seq a -> Int -> a
xs ! i = Maybe.fromJust (xs Seq.!? i)

parser :: Parsec Text () Burrow
parser = do
  _ <- string "#############\n"
  _ <- string "#"
  hallway <- map (const Nothing) <$> many1 (char '.')
  _ <- string "#\n"
  _ <- string "###"
  room1X <- amphipod
  _ <- string "#"
  room2X <- amphipod
  _ <- string "#"
  room3X <- amphipod
  _ <- string "#"
  room4X <- amphipod
  _ <- string "###\n"
  _ <- string "  #"
  room1Y <- amphipod
  _ <- string "#"
  room2Y <- amphipod
  _ <- string "#"
  room3Y <- amphipod
  _ <- string "#"
  room4Y <- amphipod
  _ <- string "#\n"
  _ <- string "  #########"
  return $
    Burrow
      (Seq.fromList [Room A [room1X, D, D, room1Y], Room B [room2X, C, B, room2Y], Room C [room3X, B, A, room3Y], Room D [room4X, A, C, room4Y]])
      (Seq.fromList hallway)
  where
    amphipod =
      choice
        [ char 'A' $> A,
          char 'B' $> B,
          char 'C' $> C,
          char 'D' $> D
        ]
