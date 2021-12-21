{-# OPTIONS -Wall #-}

import Control.Monad.State.Strict
import Data.Text (Text)
import Helpers.Parse
import Text.Parsec hiding (State)

data GameState = GameState
  { stateDie :: [Int],
    stateDieRolls :: Int
  }

main :: IO ()
main = do
  (player1StartingPosition, player2StartingPosition) <- parseTextIO parser
  let die = cycle [1 .. 100]
  let (losingScore, dieRolls) = play die player1StartingPosition player2StartingPosition
  print $ losingScore * dieRolls

play :: [Int] -> Int -> Int -> (Int, Int)
play die player1StartingPosition player2StartingPosition =
  let ((winningPlayerScore, losingPlayerScore), GameState _ dieRolls) =
        flip runState newGameState $ play' player1StartingPosition 0 player2StartingPosition 0
   in (min winningPlayerScore losingPlayerScore, dieRolls)
  where
    newGameState = GameState {stateDie = die, stateDieRolls = 0}
    play' :: Int -> Int -> Int -> Int -> State GameState (Int, Int)
    play' playerPosition playerScore nextPlayerPosition nextPlayerScore = do
      rolls <- replicateM 3 roll
      let newPosition = (playerPosition + sum rolls - 1) `mod` 10 + 1
      let newScore = playerScore + newPosition
      if newScore >= 1000
        then return (newScore, nextPlayerScore)
        else play' nextPlayerPosition nextPlayerScore newPosition newScore

roll :: State GameState Int
roll = state $ \(GameState (nextRoll : die) dieRolls) -> (nextRoll, GameState die (succ dieRolls))

parser :: Parsec Text () (Int, Int)
parser = do
  _ <- string "Player 1 starting position: "
  player1StartingPosition <- int
  _ <- string "\n"
  _ <- string "Player 2 starting position: "
  player2StartingPosition <- int
  _ <- string "\n"
  return (player1StartingPosition, player2StartingPosition)
