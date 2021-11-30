-- The first floor contains a polonium generator, a thulium generator, a thulium-compatible microchip, a promethium generator, a ruthenium generator, a ruthenium-compatible microchip, a cobalt generator, and a cobalt-compatible microchip.
-- The second floor contains a polonium-compatible microchip and a promethium-compatible microchip.
-- The third floor contains nothing relevant.
-- The fourth floor contains nothing relevant.
{-# LANGUAGE DeriveGeneric #-}

import Control.Monad (mapM_)
import Data.Array as Array
import Data.HashSet as Set
import Data.Hashable (Hashable, hashWithSalt)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import GHC.Generics (Generic)

instance (Hashable i, Hashable e) => Hashable (Array i e) where
  hashWithSalt salt array = foldl hashWithSalt (hashWithSalt salt (bounds array)) (Array.elems array)

instance (Hashable a) => Hashable (Set a) where
  hashWithSalt salt set = foldl hashWithSalt salt (Set.elems set)

data Item = Generator Element | Microchip Element
  deriving (Eq, Ord, Generic)

instance Show Item where
  show (Generator element) = show element ++ "G"
  show (Microchip element) = show element ++ "M"

instance Hashable Item

data Element = Cobalt | Dilithium | Elerium | Polonium | Promethium | Ruthenium | Thulium
  deriving (Eq, Ord, Generic)

instance Show Element where
  show Cobalt = "C"
  show Dilithium = "D"
  show Elerium = "E"
  show Polonium = "O"
  show Promethium = "P"
  show Ruthenium = "R"
  show Thulium = "T"

instance Hashable Element

type Moves = Int

type Floor = Int

type Placement = Array Floor (Set Item)

data State = State {stateMoves :: Moves, stateFloor :: Floor, statePlacement :: Placement}
  deriving (Eq)

floors :: (Floor, Floor)
floors = (1, 4)

input :: State
input =
  ( State
      0
      (fst floors)
      ( listArray
          floors
          [ fromList
              [ Generator Cobalt,
                Microchip Cobalt,
                Generator Dilithium,
                Microchip Dilithium,
                Generator Elerium,
                Microchip Elerium,
                Generator Polonium,
                Generator Promethium,
                Generator Ruthenium,
                Microchip Ruthenium,
                Generator Thulium,
                Microchip Thulium
              ],
            fromList
              [ Microchip Polonium,
                Microchip Promethium
              ],
            empty,
            empty
          ]
      )
  )

expected :: Placement
expected =
  listArray
    floors
    [ empty,
      empty,
      empty,
      fromList
        [ Generator Cobalt,
          Microchip Cobalt,
          Generator Dilithium,
          Microchip Dilithium,
          Generator Elerium,
          Microchip Elerium,
          Generator Polonium,
          Microchip Polonium,
          Generator Promethium,
          Microchip Promethium,
          Generator Ruthenium,
          Microchip Ruthenium,
          Generator Thulium,
          Microchip Thulium
        ]
    ]

main = do
  let iterations = solve [input] (singleton (stateFloor input, statePlacement input))
  let solution = List.find solved iterations
  print solution

solved :: State -> Bool
solved (State _ _ placement) = placement == expected

solve :: [State] -> Set (Floor, Placement) -> [State]
solve (state@(State movesMade currentFloor placement) : rest) seen =
  if solved state
    then state : solve rest seen
    else state : solve (List.map addMovesMade moves ++ rest) (seen `union` Set.fromList moves)
  where
    moves = ascendingMoves ++ descendingMoves
    ascendingMoves = move currentFloor (currentFloor + 1) [2, 1] seen placement
    descendingMoves = move currentFloor (currentFloor - 1) [1, 2] seen placement
    addMovesMade = uncurry $ State (movesMade + 1)

move :: Floor -> Floor -> [Int] -> Set (Floor, Placement) -> Placement -> [(Floor, Placement)]
move currentFloor newFloor itemCounts seen =
  List.filter (valid . snd)
    . List.filter (`notMember` seen)
    . deriveMoves currentFloor newFloor itemCounts

deriveMoves :: Floor -> Floor -> [Int] -> Placement -> [(Floor, Placement)]
deriveMoves currentFloor newFloor itemCounts placement =
  if not (inRange floors newFloor)
    then []
    else List.map (\items -> (newFloor, placement // update items)) itemsToMove
  where
    floorItems = placement ! currentFloor
    itemsToMove = concatMap (\itemCount -> combinations itemCount floorItems) itemCounts
    update items = [(newFloor, (placement ! newFloor) `union` items), (currentFloor, floorItems `difference` items)]

valid :: Placement -> Bool
valid placement = all validFloor (Array.elems placement)

validFloor :: Set Item -> Bool
validFloor items = all microchipSafe (Set.toList microchips)
  where
    (microchips, generators) = Set.partition isMicrochip items
    microchipSafe (Microchip element) =
      let (mine, others) = Set.partition (== Generator element) generators
       in Set.null mine || not (Set.null others)

isMicrochip :: Item -> Bool
isMicrochip (Microchip _) = True
isMicrochip _ = False

combinations :: (Ord a, Hashable a) => Int -> Set a -> [Set a]
combinations n set = List.map Set.fromList $ combinations' n (Set.toList set)
  where
    combinations' 0 _ = [[]]
    combinations' n list = [x : xs | x : ts <- List.tails list, xs <- combinations' (n - 1) ts]

----------------------------------------

instance Show State where
  show (State movesMade floor placement) =
    show movesMade ++ "\n"
      ++ concatFor
        (reverse $ assocs placement)
        ( \(f, items) ->
            (if f == floor then " * " else "   ") ++ concatFor (Set.elems items) (\item -> show item ++ " ") ++ "\n"
        )
    where
      concatFor = flip concatMap

display :: [State] -> IO ()
display iterations =
  mapM_ print (sample iterations)

sample :: [a] -> [a]
sample [] = []
sample (x : xs) = x : sample (drop 100 xs)
