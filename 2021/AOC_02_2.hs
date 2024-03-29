{-# OPTIONS -Wall #-}
{-# LANGUAGE NamedFieldPuns #-}

import Helpers.Parse
import Text.Parsec

data Command = Forward Int | Down Int | Up Int
  deriving (Show)

data Position = Position {horizontal :: Int, depth :: Int, aim :: Int}
  deriving (Show)

main :: IO ()
main = do
  commands <- parseInput
  let Position {horizontal, depth} = foldl move initialPosition commands
  let answer = horizontal * depth
  print answer

initialPosition :: Position
initialPosition = Position {horizontal = 0, depth = 0, aim = 0}

move :: Position -> Command -> Position
move position (Forward n) =
  position
    { horizontal = horizontal position + n,
      depth = depth position + aim position * n
    }
move position (Up n) = position {aim = aim position - n}
move position (Down n) = position {aim = aim position + n}

parseInput :: IO [Command]
parseInput = parseLinesIO $ do
  constructor <-
    try (string "forward" >> pure Forward)
      <|> try (string "down" >> pure Down)
      <|> try (string "up" >> pure Up)
  _ <- many1 space
  amount <- read <$> many1 digit
  return $ constructor amount
