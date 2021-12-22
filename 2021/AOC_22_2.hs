{-# OPTIONS -Wall #-}

import Data.Functor
import qualified Data.List as List
import Data.Text (Text)
import Helpers.Parse
import Text.Parsec

data Instruction = Instruction Switch Cube
  deriving (Show)

data Switch = Off | On
  deriving (Show)

data Cube = Cube Range Range Range
  deriving (Eq)

instance Show Cube where
  show (Cube x y z) = "(x=" ++ show x ++ ",y=" ++ show y ++ ",z=" ++ show z ++ ")"

data Range = Range Int Int
  deriving (Eq)

instance Show Range where
  show (Range start end) = show start ++ ".." ++ show end

main :: IO ()
main = do
  instructions <- parseLinesIO parser
  let cubes = List.foldl' apply [] instructions
  print $ sum $ map size cubes

apply :: [Cube] -> Instruction -> [Cube]
apply onCubes (Instruction Off cube) =
  concatMap (`without` cube) onCubes
apply onCubes (Instruction On cube) =
  cube : concatMap (`without` cube) onCubes

without :: Cube -> Cube -> [Cube]
a@(Cube aX aY aZ) `without` b@(Cube bX bY bZ) =
  let xs = if aX `overlapsWith` bX then splitX bX a else [a]
      ys = if aY `overlapsWith` bY then concatMap (splitY bY) xs else xs
      zs = if aZ `overlapsWith` bZ then concatMap (splitZ bZ) ys else ys
   in mergeCubes $ List.delete (constrainCubeTo a b) zs
  where
    splitX range (Cube x y z) = map (\x' -> Cube x' y z) (splitRange range x)
    splitY range (Cube x y z) = map (\y' -> Cube x y' z) (splitRange range y)
    splitZ range (Cube x y z) = map (Cube x y) (splitRange range z)

mergeCubes :: [Cube] -> [Cube]
mergeCubes cubes =
  case List.find (\((_, a), (_, b)) -> mergeable a b) pairs of
    Nothing -> cubes
    Just ((aI, a), (bI, b)) -> mergeCubes $ (merge a b :) $ deleteAt aI $ deleteAt bI cubes
  where
    pairs = [((aI, cubes !! aI), (bI, cubes !! bI)) | aI <- [0 .. length cubes - 1], bI <- [aI + 1 .. length cubes - 1]]
    mergeable (Cube aX aY aZ) (Cube bX bY bZ) =
      aX == bX && aY == bY && contiguous aZ bZ
        || aX == bX && aZ == bZ && contiguous aY bY
        || aY == bY && aZ == bZ && contiguous aX bX
    contiguous (Range aStart aEnd) (Range bStart bEnd) =
      aEnd + 1 == bStart || bEnd + 1 == aStart
    merge (Cube aX aY aZ) (Cube bX bY bZ) =
      Cube (connect aX bX) (connect aY bY) (connect aZ bZ)
    connect (Range aStart aEnd) (Range bStart bEnd) =
      if aStart < bStart
        then Range aStart bEnd
        else Range bStart aEnd
    deleteAt _ [] = error "deleteAt: Out of range."
    deleteAt 0 (_ : xs) = xs
    deleteAt n (x : xs) = x : deleteAt (n - 1) xs

constrainCubeTo :: Cube -> Cube -> Cube
constrainCubeTo (Cube aX aY aZ) (Cube bX bY bZ) =
  Cube (constrainRangeTo aX bX) (constrainRangeTo aY bY) (constrainRangeTo aZ bZ)

constrainRangeTo :: Range -> Range -> Range
constrainRangeTo (Range aStart aEnd) (Range bStart bEnd) =
  Range (max aStart bStart) (min aEnd bEnd)

splitRange :: Range -> Range -> [Range]
splitRange a@(Range aStart aEnd) b@(Range bStart bEnd) =
  case (aStart <= bStart, aEnd >= bEnd) of
    (True, True) -> [b]
    (True, False) -> [Range bStart aEnd, Range (aEnd + 1) bEnd]
    (False, True) -> [Range bStart (aStart - 1), Range aStart bEnd]
    (False, False) -> [Range bStart (aStart - 1), a, Range (aEnd + 1) bEnd]

overlapsWith :: Range -> Range -> Bool
Range aStart aEnd `overlapsWith` Range bStart bEnd =
  aStart <= bStart && aEnd >= bStart
    || aStart <= bEnd && aEnd >= bEnd

size :: Cube -> Int
size (Cube xRange yRange zRange) = rangeLength xRange * rangeLength yRange * rangeLength zRange
  where
    rangeLength (Range start end) = end - start + 1

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
