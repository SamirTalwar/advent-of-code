import Data.Char qualified as Char
import Data.Set (Set)
import Data.Set qualified as Set

data Position = Position {posY :: Int, posX :: Int}
  deriving (Eq, Ord, Show)

data Number = Number {numberValue :: Int, numberY :: Int, numberXStart :: Int, numberXEnd :: Int}
  deriving (Eq, Show)

main :: IO ()
main = do
  gridLines <- zip [0 ..] . map (zip [0 ..]) . lines <$> getContents
  let numbers = concatMap (uncurry parseNumbers) gridLines
      symbols = Set.fromList $ concatMap (uncurry parseSymbols) gridLines
      adjacents = map (\n -> (n, positionsAdjacentTo n)) numbers
      partNumbers = map fst $ filter (not . (symbols `Set.disjoint`) . snd) adjacents
      result = sum $ map numberValue partNumbers
  print result

positionsAdjacentTo :: Number -> Set Position
positionsAdjacentTo (Number _ y xStart xEnd) =
  Set.fromList $
    Position y (xStart - 1)
      : Position y (xEnd + 1)
      : concatMap (\x -> [Position (y - 1) x, Position (y + 1) x]) [xStart - 1 .. xEnd + 1]

parseNumbers :: Int -> [(Int, Char)] -> [Number]
parseNumbers _ [] = []
parseNumbers y line
  | Char.isDigit (snd (head line)) =
      let (numberSegment, rest) = span (Char.isDigit . snd) line
          number =
            Number
              { numberValue = read (map snd numberSegment),
                numberY = y,
                numberXStart = fst (head numberSegment),
                numberXEnd = fst (last numberSegment)
              }
       in number : parseNumbers y rest
  | otherwise = parseNumbers y (tail line)

parseSymbols :: Int -> [(Int, Char)] -> [Position]
parseSymbols _ [] = []
parseSymbols y ((x, c) : rest)
  | Char.isDigit c || c == '.' = parseSymbols y rest
  | otherwise = Position y x : parseSymbols y rest
