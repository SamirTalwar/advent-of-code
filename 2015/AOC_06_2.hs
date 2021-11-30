{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Array
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text

newtype Light = Brightness Int
  deriving (Eq, Num, Show)

type Lights = Array Coordinates Light

data Instruction = Instruction Command Coordinates Coordinates
  deriving (Eq, Show)

data Command = TurnOn | TurnOff | Toggle
  deriving (Eq, Show)

type Coordinates = (Int, Int)

lightsBounds :: (Coordinates, Coordinates)
lightsBounds = ((0, 0), (999, 999))

main = do
  instructions <- map parseInput <$> Text.lines <$> IO.getContents
  let finalState = foldl (flip apply) initialState instructions
  let count = sum $ elems finalState
  print count

parseInput :: Text -> Instruction
parseInput text = either (error . show) id $ parse parser "" text
  where
    parser = do
      command <- instructionCommand
      space
      startX <- number
      char ','
      startY <- number
      string " through "
      endX <- number
      char ','
      endY <- number
      return $ Instruction command (startX, startY) (endX, endY)
    instructionCommand =
      (try (string "turn on") >> return TurnOn)
        <|> (try (string "turn off") >> return TurnOff)
        <|> (try (string "toggle") >> return Toggle)
    number = read <$> many1 digit

initialState :: Lights
initialState = listArray lightsBounds (repeat $ Brightness 0)

apply :: Instruction -> Lights -> Lights
apply (Instruction command start end) lights =
  accum (flip ($)) lights $ map (\i -> (i, decode command)) (range (start, end))

decode :: Command -> (Light -> Light)
decode TurnOn (Brightness n) = Brightness $ n + 1
decode TurnOff (Brightness n) = Brightness $ max 0 (n - 1)
decode Toggle (Brightness n) = Brightness $ n + 2
