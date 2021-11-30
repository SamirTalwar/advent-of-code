import qualified Data.Char as Char
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Text.Parsec
import Text.Parsec.String

newtype Molecule = Molecule {unMolecule :: [String]}
  deriving (Eq, Ord, Show)

type Replacements = Map Molecule [Molecule]

main = do
  input <- lines <$> getContents
  let replacements = multimap $ map parseInput $ takeWhile (not . null) input
  let medicine = parseMolecule $ last input
  let generated = Set.fromList $ step replacements medicine
  print $ Set.size generated

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

step :: Replacements -> Molecule -> [Molecule]
step _ (Molecule []) = []
step replacements (Molecule molecule@(m : ms))
  | bit `Map.member` replacements =
    map (`appendMolecule` rest) (replacements ! bit) ++ andSoOn
  | otherwise =
    andSoOn
  where
    bit = Molecule [m]
    rest = Molecule ms
    andSoOn = map (bit `appendMolecule`) $ step replacements rest

multimap :: Ord k => [(k, v)] -> Map k [v]
multimap = foldr insert Map.empty
  where
    insert (key, value) = Map.alter (update value) key
    update value Nothing = Just [value]
    update value (Just values) = Just (value : values)
