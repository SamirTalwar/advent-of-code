{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text

data Command = Forward Int | Down Int | Up Int
  deriving (Show)

data Position = Position {horizontal :: Int, depth :: Int}

main = do
  commands <- map parseInput . Text.lines <$> IO.getContents
  let Position {horizontal, depth} = foldl move initialPosition commands
  let answer = horizontal * depth
  print answer

initialPosition :: Position
initialPosition = Position {horizontal = 0, depth = 0}

move :: Position -> Command -> Position
move position (Forward n) = position {horizontal = horizontal position + n}
move position (Up n) = position {depth = depth position - n}
move position (Down n) = position {depth = depth position + n}

parseInput :: Text -> Command
parseInput = either (error . show) id . parse parser ""
  where
    parser = do
      constructor <-
        try (string "forward" >> pure Forward)
          <|> try (string "down" >> pure Down)
          <|> try (string "up" >> pure Up)
      many1 space
      amount <- read <$> many1 digit
      return $ constructor amount
