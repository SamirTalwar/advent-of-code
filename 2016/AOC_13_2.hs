import Control.Monad (forM_)
import qualified Data.Bits as Bits
import qualified Data.Set as Set

data Location = OpenSpace | Wall
  deriving (Eq)

instance Show Location where
  show OpenSpace = "."
  show Wall = "#"

type Coordinates = (Int, Int)

newtype FavoriteNumber = FavoriteNumber Int
  deriving (Eq, Show)

main = do
  fav <- FavoriteNumber <$> read <$> getContents
  let paths = go fav limit [start]
  let coordinates = foldl Set.union Set.empty $ map Set.fromList paths
  forM_ [0 .. 29] $ \y ->
    putStrLn $ concatMap (\c -> if c `Set.member` coordinates then "O" else show (location fav c)) [(x, y) | x <- [0 .. 29]]
  print $ Set.size coordinates

start = (1, 1)

limit = 50

go :: FavoriteNumber -> Int -> [Coordinates] -> [[Coordinates]]
go _ 0 history = [history]
go fav limit history = if null moves then [history] else concatMap (go fav (limit - 1)) moves
  where
    moves = movements fav history

movements :: FavoriteNumber -> [Coordinates] -> [[Coordinates]]
movements fav history@(current : _) = map (: history) $ filter (valid fav history) $ around current

around :: Coordinates -> [Coordinates]
around (x, y) =
  [ (x, y + 1),
    (x + 1, y),
    (x, y - 1),
    (x - 1, y)
  ]

valid :: FavoriteNumber -> [Coordinates] -> Coordinates -> Bool
valid fav history coordinates@(x, y) =
  x >= 0 && y >= 0
    && coordinates `notElem` history
    && location fav coordinates == OpenSpace

location :: FavoriteNumber -> Coordinates -> Location
location (FavoriteNumber fav) (x, y) = if even magicNumber then OpenSpace else Wall
  where
    magicNumber = Bits.popCount $ x * x + 3 * x + 2 * x * y + y + y * y + fav
