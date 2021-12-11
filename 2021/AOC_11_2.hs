{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.List as List
import qualified Data.Map.Lazy as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers.Function
import Helpers.Grid (Grid)
import qualified Helpers.Grid as Grid

main :: IO ()
main = do
  levels <- Grid.fromDigits <$> getContents
  let steps = iterate (step . fst) (levels, 0)
  let answer = Maybe.fromJust $ List.findIndex (Grid.all (== 0) . fst) steps
  print answer

step :: Grid Int -> (Grid Int, Int)
step startingLevels = step' (succ <$> startingLevels) Set.empty
  where
    step' :: Grid Int -> Set (Int, Int) -> (Grid Int, Int)
    step' levels flashed =
      let updatedFlashed = Set.fromList (Grid.coordinatesWhere (> 9) levels)
          newFlashed = updatedFlashed Set.\\ flashed
       in if Set.size newFlashed == 0
            then
              let updates = map (,0) (Set.toList flashed)
               in (levels Grid.// updates, Set.size flashed)
            else
              let updates =
                    Set.toList newFlashed
                      |> concatMap (`Grid.neighboringCoordinatesWithDiagonals` levels)
                      |> map (,1)
                      |> Map.fromListWith (+)
                      |> Map.toList
               in step' (Grid.updateWith (+) updates levels) updatedFlashed
