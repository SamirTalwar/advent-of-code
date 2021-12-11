{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.List as List
import qualified Data.Map.Lazy as Map
import qualified Data.Maybe as Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers.Function
import Helpers.Matrix (Matrix)
import qualified Helpers.Matrix as Matrix

main :: IO ()
main = do
  levels <- Matrix.fromDigits <$> getContents
  let steps = iterate (step . fst) (levels, 0)
  let answer = Maybe.fromJust $ List.findIndex (Matrix.all (== 0) . fst) steps
  print answer

step :: Matrix Int -> (Matrix Int, Int)
step startingLevels = step' (succ <$> startingLevels) Set.empty
  where
    step' :: Matrix Int -> Set (Int, Int) -> (Matrix Int, Int)
    step' levels flashed =
      let updatedFlashed = Set.fromList (Matrix.coordinatesWhere (> 9) levels)
          newFlashed = updatedFlashed Set.\\ flashed
       in if Set.size newFlashed == 0
            then
              let updates = map (,0) (Set.toList flashed)
               in (levels Matrix.// updates, Set.size flashed)
            else
              let updates =
                    Set.toList newFlashed
                      |> concatMap (`Matrix.neighboringCoordinatesWithDiagonals` levels)
                      |> map (,1)
                      |> Map.fromListWith (+)
                      |> Map.toList
               in step' (Matrix.updateWith (+) updates levels) updatedFlashed
