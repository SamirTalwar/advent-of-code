import           Data.Ratio
import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import           Text.Parsec
import           Text.Parsec.Text

data Disk = Disk {
    location :: Coordinate,
    size :: Size,
    used :: Size,
    available :: Size,
    percentageUsed :: Ratio Int
  } deriving (Eq, Show)
type Coordinate = (Int, Int)
newtype Size = Terabyte Int
  deriving (Eq, Ord)

instance Show Size where
  show (Terabyte magnitude) = show magnitude ++ "T"

main = do
  input <- Text.lines <$> IO.getContents
  let disks = map parseInput $ drop 2 input
  let viablePairsOfDisks = viablePairs disks
  print $ length viablePairsOfDisks

parseInput :: Text -> Disk
parseInput text = either (error . show) id $ parse parser "" text
  where
  parser = do
    string "/dev/grid/node-x"
    x <- number
    string "-y"
    y <- number
    whitespace
    diskSize <- size
    whitespace
    used <- size
    whitespace
    available <- size
    whitespace
    percentageUsed <- percentage
    return $ Disk (x, y) diskSize used available percentageUsed
  size = do
    magnitude <- number
    char 'T'
    return $ Terabyte magnitude
  percentage = do
    magnitude <- number
    char '%'
    return (magnitude % 100)
  number = read <$> many1 digit
  whitespace = many1 space

viablePairs :: [Disk] -> [(Disk, Disk)]
viablePairs disks = filter (uncurry viable) $ pairs disks
  where
  viable a b = (used a > Terabyte 0) && (used a <= available b)

pairs :: [a] -> [(a, a)]
pairs = map (\[a, b] -> (a, b)) . permutations 2

permutations :: Int -> [a] -> [[a]]
permutations 0 _ = [[]]
permutations n list = concatMap (\(x, xs) -> map (x :) $ permutations (n - 1) xs) $ pick list
  where
  pick :: [a] -> [(a, [a])]
  pick = pick' []
  pick' before [] = []
  pick' before (current : after) = (current, before ++ after) : pick' (before ++ [current]) after
