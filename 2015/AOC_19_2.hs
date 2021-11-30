-- Wow, this is slow. Can't figure out a faster version though.

import qualified Data.Char as Char
import qualified Data.List as List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Ord as Ord
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace
import Text.Parsec
import Text.Parsec.String

newtype Molecule = Molecule {unMolecule :: [String]}
  deriving (Eq, Ord)

type Replacements = Map Molecule [Molecule]

instance Show Molecule where
  show (Molecule bits) = concat bits

start = Molecule ["e"]

main = do
  input <- lines <$> getContents
  let replacements = multimap $ map parseInput $ takeWhile (not . null) input
  let medicine = parseMolecule $ last input
  let iterations = zip [0 ..] $ repeatedlyStep replacements medicine start
  let (Just result) = List.find (\(i, (closeness, molecules)) -> traceShow (i, closeness, length molecules) $ medicine `elem` molecules) iterations
  print $ fst result

parseInput :: String -> (Molecule, Molecule)
parseInput text = either (error . show) id $ parse parser "" text
  where
    parser = do
      input <- molecule
      string " => "
      output <- molecule
      return (input, output)
    molecule = parseMolecule <$> many1 letter

parseMolecule :: String -> Molecule
parseMolecule = Molecule . parseMolecule'
  where
    parseMolecule' "" = []
    parseMolecule' [x] = [[x]]
    parseMolecule' (x : xs@(y : ys))
      | Char.isLower y = [x, y] : parseMolecule' ys
      | otherwise = [x] : parseMolecule' xs

appendMolecule :: Molecule -> Molecule -> Molecule
appendMolecule (Molecule a) (Molecule b) = Molecule (a ++ b)

repeatedlyStep :: Replacements -> Molecule -> Molecule -> [(Int, [Molecule])]
repeatedlyStep replacements target start =
  iterate
    ( closest target
        . Set.unions
        . map (step replacements target)
        . snd
    )
    (0, [start])

step :: Replacements -> Molecule -> Molecule -> Set Molecule
step replacements target molecule = Set.fromList $ step' molecule
  where
    step' (Molecule []) = []
    step' molecule =
      case Map.lookup current replacements of
        Nothing -> []
        Just currentReplacements ->
          map (\replacement -> previous `appendMolecule` replacement `appendMolecule` future) currentReplacements
      where
        (sm, (m : ms)) = List.splitAt (lengthOfCommonPart target molecule) (unMolecule molecule)
        previous = Molecule sm
        current = Molecule [m]
        future = Molecule ms

closest :: Molecule -> Set Molecule -> (Int, [Molecule])
closest target candidates = (bestLength, best)
  where
    targetLength = length $ unMolecule target
    best = map fst $ takeWhile ((\l -> l `elem` [(bestLength - 8) .. bestLength]) . snd) comparisons
    bestLength = snd $ head comparisons
    comparisons =
      List.sortBy (flip (Ord.comparing snd)) $
        map (\candidate -> (candidate, lengthOfCommonPart target candidate)) $
          Set.toList valid
    valid = Set.filter (\candidate -> length (unMolecule candidate) <= targetLength) candidates

lengthOfCommonPart (Molecule target) candidate@(Molecule bits) =
  length $
    takeWhile (uncurry (==)) $
      zip target bits

multimap :: Ord k => [(k, v)] -> Map k [v]
multimap = foldr insert Map.empty
  where
    insert (key, value) = Map.alter (update value) key
    update value Nothing = Just [value]
    update value (Just values) = Just (value : values)
