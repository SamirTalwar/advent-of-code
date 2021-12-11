{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers.Function
import Helpers.Grid (Grid, Point)
import qualified Helpers.Grid as Grid

stepCount :: Int
stepCount = 100

main :: IO ()
main = do
  levels <- Grid.fromDigits <$> getContents
  let steps = iterate (step . fst) (levels, 0)
  let answer = sum $ take (stepCount + 1) $ map snd steps
  print answer

step :: Grid Int -> (Grid Int, Int)
step startingLevels = step' (succ <$> startingLevels) Set.empty
  where
    step' :: Grid Int -> Set Point -> (Grid Int, Int)
    step' levels flashed =
      let updatedFlashed = Grid.pointsWhere (> 9) levels
          newFlashed = updatedFlashed Set.\\ flashed
       in if Set.size newFlashed == 0
            then
              let updates = map (,0) (Set.toList flashed)
               in (levels Grid.// updates, Set.size flashed)
            else
              let updates =
                    Set.toList newFlashed
                      |> map (Map.fromSet (const 1) . (`Grid.neighboringPointsWithDiagonals` levels))
                      |> Map.unionsWith (+)
               in step' (Grid.updateWith (+) updates levels) updatedFlashed
