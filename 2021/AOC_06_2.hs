{-# OPTIONS -Wall #-}

import Data.Map (Map)
import qualified Data.Map as Map
import Helpers.Function
import Helpers.Map
import Helpers.Parse
import Text.Parsec

days :: Int
days = 256

main :: IO ()
main = do
  input <- parseTextIO $ sepBy int (string ",")
  let initialState = countValues input
  let finalState = iterate step initialState !! days
  print $ sum $ map snd $ Map.toList finalState

step :: Map Int Int -> Map Int Int
step state = Map.foldlWithKey' stepFish state state
  where
    stepFish :: Map Int Int -> Int -> Int -> Map Int Int
    stepFish currentState 0 c =
      currentState
        |> Map.update (decrease c) 0
        |> Map.insertWith (+) 8 c
        |> Map.insertWith (+) 6 c
    stepFish currentState n c =
      currentState
        |> Map.update (decrease c) n
        |> Map.insertWith (+) (pred n) c
    decrease :: Int -> Int -> Maybe Int
    decrease b a
      | a <= b = Nothing
      | otherwise = Just (a - b)
