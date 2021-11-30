import Data.Bits
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text

type Connection = (Wire, Operation)

type Connections = Map Wire Operation

newtype Wire = Wire String
  deriving (Eq, Ord, Show)

data Input = Signal Value | InputWire Wire
  deriving (Eq, Show)

data Operation
  = Input Input
  | And Input Input
  | Or Input Input
  | LeftShift Input Int
  | RightShift Input Int
  | Not Input
  deriving (Eq, Show)

type Value = Integer

type Values = Map Wire Value

wire = Wire "a"

overrideWire = Wire "b"

main = do
  connections <- Map.fromList <$> map parseInput <$> Text.lines <$> IO.getContents
  let values = compute connections
  let overriddenConnections = Map.insert overrideWire (Input $ Signal $ values ! wire) connections
  let overriddenValues = compute overriddenConnections
  print (overriddenValues ! wire)

parseInput :: Text -> Connection
parseInput text = either (error . show) id $ parse parser "" text
  where
    parser = do
      op <- operation
      string " -> "
      w <- wire
      return $ (w, op)
    operation = choice $ map try [opAnd, opOr, opLeftShift, opRightShift, opNot, Input <$> input]
    input = try signal <|> try inputWire
    signal = Signal <$> value
    inputWire = InputWire <$> wire
    opAnd = do
      a <- input
      space
      string "AND"
      space
      b <- input
      return $ And a b
    opOr = do
      a <- input
      space
      string "OR"
      space
      b <- input
      return $ Or a b
    opLeftShift = do
      i <- input
      space
      string "LSHIFT"
      space
      amount <- int
      return $ LeftShift i amount
    opRightShift = do
      i <- input
      space
      string "RSHIFT"
      space
      amount <- int
      return $ RightShift i amount
    opNot = do
      string "NOT"
      space
      i <- input
      return $ Not i
    wire = Wire <$> many1 letter
    value = read <$> many1 digit
    int = read <$> many1 digit

compute :: Connections -> Values
compute connections = values
  where
    values :: Values
    values = Map.map value connections
    value :: Operation -> Value
    value (Input input) = inputValue input
    value (And a b) = inputValue a .&. inputValue b
    value (Or a b) = inputValue a .|. inputValue b
    value (LeftShift input amount) = inputValue input `shiftL` amount
    value (RightShift input amount) = inputValue input `shiftR` amount
    value (Not input) = complement (inputValue input)
    inputValue :: Input -> Value
    inputValue (Signal signalValue) = signalValue
    inputValue (InputWire wire) = values ! wire
