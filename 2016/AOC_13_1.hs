import Control.Monad (forM_, mapM_)
import qualified Data.Bits as Bits

data Location = OpenSpace | Wall
  deriving (Eq)

type Coordinates = (Int, Int)

newtype FavoriteNumber = FavoriteNumber Int

main = do
  fav <- FavoriteNumber <$> read <$> getContents
  print $ go fav start finish

start = (1, 1)

finish = (31, 39)

go :: FavoriteNumber -> Coordinates -> Coordinates -> Int
go fav start finish = go' finish [[start]]
  where
    go' destination (history@(current : past) : next) =
      if current == destination
        then length history - 1
        else go' destination (next ++ movements fav history)

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
