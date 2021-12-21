{-# OPTIONS -Wall #-}

import Control.Monad.State.Strict
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Helpers.Map
import Helpers.Memoization
import Helpers.Parse
import Text.Parsec hiding (State)

main :: IO ()
main = do
  (player1StartingPosition, player2StartingPosition) <- parseTextIO parser
  let answer = play player1StartingPosition player2StartingPosition
  print answer

play :: Int -> Int -> Int
play player1StartingPosition player2StartingPosition =
  let (player1Won, player2Won) = memoPlay' player1StartingPosition 0 player2StartingPosition 0
   in max player1Won player2Won
  where
    memoPlay' = memo4 play'
    play' :: Int -> Int -> Int -> Int -> (Int, Int)
    play' playerPosition playerScore nextPlayerPosition nextPlayerScore =
      let winners = flip map quantumRolls $ \(rollSum, rollCount) ->
            let newPosition = (playerPosition + rollSum - 1) `mod` 10 + 1
                newScore = playerScore + newPosition
                (nextPlayerWins, playerWins) =
                  if newScore >= 21
                    then (0, 1)
                    else memoPlay' nextPlayerPosition nextPlayerScore newPosition newScore
             in (playerWins * rollCount, nextPlayerWins * rollCount)
       in List.foldl' (\(player1Wins, player2Wins) (player1Won, player2Won) -> (player1Won + player1Wins, player2Won + player2Wins)) (0, 0) winners

quantumRolls :: [(Int, Int)]
quantumRolls = Map.toList $ countValues $ map sum $ replicateM 3 roll

roll :: [Int]
roll = [1, 2, 3]

parser :: Parsec Text () (Int, Int)
parser = do
  _ <- string "Player 1 starting position: "
  player1StartingPosition <- int
  _ <- string "\n"
  _ <- string "Player 2 starting position: "
  player2StartingPosition <- int
  _ <- string "\n"
  return (player1StartingPosition, player2StartingPosition)
