{-# OPTIONS -Wall #-}

import Helpers.Numbers (triangular)
import Helpers.Parse
import Text.Parsec

main :: IO ()
main = do
  input <- parseTextIO $ sepBy int (string ",")
  let start = sum input `div` length input
  let upAnswer = findAnswerUp start input
  let downAnswer = findAnswerDown start input
  print $ fst $ min upAnswer downAnswer

findAnswerUp :: Int -> [Int] -> (Int, Int)
findAnswerUp currentTarget positions =
  let distance = computeDistance currentTarget positions
   in findAnswer currentTarget positions distance succ

findAnswerDown :: Int -> [Int] -> (Int, Int)
findAnswerDown currentTarget positions =
  let distance = computeDistance currentTarget positions
   in findAnswer currentTarget positions distance pred

findAnswer :: Int -> [Int] -> Int -> (Int -> Int) -> (Int, Int)
findAnswer currentTarget positions distance next =
  let nextTarget = next currentTarget
      nextDistance = computeDistance nextTarget positions
   in if nextDistance > distance
        then (distance, currentTarget)
        else findAnswer nextTarget positions nextDistance next

computeDistance :: Int -> [Int] -> Int
computeDistance target = sum . map (\p -> triangular (abs (target - p)))
