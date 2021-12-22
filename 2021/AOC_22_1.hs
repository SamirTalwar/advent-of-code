{-# OPTIONS -Wall #-}

import Data.Functor
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Helpers.Parse
import Text.Parsec

data Instruction = Instruction Switch Cube
  deriving (Show)

data Switch = Off | On
  deriving (Show)

data Cube = Cube Range Range Range

instance Show Cube where
  show (Cube x y z) = "x=" ++ show x ++ ", y=" ++ show y ++ ", z=" ++ show z

data Range = Range Int Int

instance Show Range where
  show (Range start end) = show start ++ ".." ++ show end

data Point = Point Int Int Int
  deriving (Eq, Ord)

instance Show Point where
  show (Point x y z) = "x=" ++ show x ++ ", y=" ++ show y ++ ", z=" ++ show z

main :: IO ()
main = do
  instructions <- parseLinesIO parser
  let validInstructions = filter inBounds instructions
  let result = List.foldl' apply Set.empty validInstructions
  print $ Set.size result

apply :: Set Point -> Instruction -> Set Point
apply points (Instruction switch (Cube xRange yRange zRange)) =
  let newPoints = Set.fromList [Point x y z | x <- inRange xRange, y <- inRange yRange, z <- inRange zRange]
   in case switch of
        Off -> points Set.\\ newPoints
        On -> points `Set.union` newPoints

inRange :: Range -> [Int]
inRange (Range start end) = [start .. end]

inBounds :: Instruction -> Bool
inBounds (Instruction _ (Cube xRange yRange zRange)) =
  inBounds' xRange && inBounds' yRange && inBounds' zRange
  where
    inBounds' (Range start end) = start >= -50 && end <= 50

parser :: Parsec Text () Instruction
parser = do
  s <- switch
  x <- string " x=" *> range
  y <- string ",y=" *> range
  z <- string ",z=" *> range
  return $ Instruction s $ Cube x y z
  where
    switch = choice [try (string "off") $> Off, try (string "on") $> On]
    range = do
      start <- int
      end <- string ".." *> int
      return $ Range start end
