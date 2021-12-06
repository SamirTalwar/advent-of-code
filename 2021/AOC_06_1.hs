{-# OPTIONS -Wall #-}

import Helpers.Parse
import Text.Parsec

days :: Int
days = 80

main :: IO ()
main = do
  initialState <- parseTextIO $ sepBy int (string ",")
  let finalState = iterate step initialState !! days
  print $ length finalState

step :: [Int] -> [Int]
step = concatMap stepFish
  where
    stepFish :: Int -> [Int]
    stepFish 0 = [6, 8]
    stepFish n = [pred n]
