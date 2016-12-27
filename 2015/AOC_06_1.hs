import           Data.Array
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import           Text.Parsec
import           Text.Parsec.Text

data Light = Off | On
  deriving (Eq, Show)
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
  let count = length $ filter (== On) $ elems finalState
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
initialState = listArray lightsBounds (repeat Off)

apply :: Instruction -> Lights -> Lights
apply (Instruction TurnOn start end) lights = lights // map (\i -> (i, On)) (range (start, end))
apply (Instruction TurnOff start end) lights = lights // map (\i -> (i, Off)) (range (start, end))
apply (Instruction Toggle start end) lights = accum (\e _ -> flipSwitch e) lights $ map (\i -> (i, ())) (range (start, end))

flipSwitch :: Light -> Light
flipSwitch Off = On
flipSwitch On = Off
