import           Crypto.Hash
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char
import           Data.Ix (inRange)
import qualified Data.List as List
import qualified Data.Ord as Ord

data Direction = U | D | L | R
  deriving (Eq)

instance Show Direction where
  show U = "U"
  show D = "D"
  show L = "L"
  show R = "R"

type Position = (Int, Int)

bounds = ((0, 0), (3, 3))

directions = [U, D, L, R]

main = do
  passcode <- strip <$> getContents
  let solution = head $ List.sortBy (Ord.comparing length) $ solve passcode [] (fst bounds)
  putStrLn $ concatMap show solution
  print $ length solution

strip :: String -> String
strip = takeWhile (not . Char.isSpace) . dropWhile Char.isSpace

solve :: String -> [Direction] -> Position -> [[Direction]]
solve passcode movements position
  | position == snd bounds = [movements]
  | otherwise = do
    direction <- validDirections
    solve passcode (movements ++ [direction]) (move position direction)
  where
  validDirections = map fst $ filter snd $ zip directions openDoors
  validDoors = map (inRange bounds . move position) directions
  openDoors = zipWith (&&) validDoors (map openDoor $ take 4 $ show $ md5 (passcode ++ concatMap show movements))
  openDoor c = c `elem` openDoorChars
  openDoorChars :: [Char]
  openDoorChars = "bcdef"

move :: Position -> Direction -> Position
move (x, y) U = (x, y - 1)
move (x, y) D = (x, y + 1)
move (x, y) L = (x - 1, y)
move (x, y) R = (x + 1, y)

md5 :: String -> Digest MD5
md5 = hash . Char8.pack
