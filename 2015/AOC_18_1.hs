import           Control.Monad (forM_)
import           Data.Array
import qualified Data.Char as Char
import qualified Data.List as List

data Light = On | Off
  deriving (Eq, Show)
type Lights = Array Coordinates Light
type Coordinates = (Int, Int)

main = do
  lightsList <- List.transpose <$> map (map parseInput) <$> lines <$> getContents
  let gridBounds = ((0, 0), (length (head lightsList) - 1, length lightsList - 1))
  let lights = listArray gridBounds $ concat lightsList
  let steps = iterate step lights
  print $ countLights (steps !! 100)

trim :: String -> String
trim = takeWhile (not . Char.isSpace) . dropWhile Char.isSpace

parseInput :: Char -> Light
parseInput '.' = Off
parseInput '#' = On

step :: Lights -> Lights
step lights = array gridBounds $ map stepLight $ assocs lights
  where
  stepLight (coordinates, state) = case switchedOnNeighbors coordinates of
    2 -> (coordinates, state)
    3 -> (coordinates, On)
    _ -> (coordinates, Off)
  switchedOnNeighbors coordinates = countSwitchedOn $ map (lights !) $ neighbours coordinates
  neighbours (x, y) =
    filter (inRange gridBounds) [
      (x - 1, y - 1),
      (x    , y - 1),
      (x + 1, y - 1),
      (x - 1, y    ),
      (x + 1, y    ),
      (x - 1, y + 1),
      (x    , y + 1),
      (x + 1, y + 1)
    ]
  gridBounds = bounds lights

countLights :: Lights -> Int
countLights = countSwitchedOn . elems

countSwitchedOn :: [Light] -> Int
countSwitchedOn = length . filter (== On)

printLights :: Lights -> IO ()
printLights lights =
  forM_ [snd (fst gridBounds) .. snd (snd gridBounds)] $ \y -> do
    forM_ [fst (fst gridBounds) .. fst (snd gridBounds)] $ \x -> do
      case lights ! (x, y) of
        Off -> putStr "."
        On -> putStr "#"
    putStrLn ""
  where
  gridBounds = bounds lights
