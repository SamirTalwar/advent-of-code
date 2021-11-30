import Data.Array
import qualified Data.Ix as Ix
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text

data Instruction
  = Copy Value Register
  | Increment Register
  | Decrement Register
  | Jump Value Value
  | Toggle Value
  | InvalidToggle Instruction Int
  deriving (Eq, Show)

type Instructions = Array Int Instruction

data Register = A | B | C | D
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

data Value
  = Direct Int
  | Indirect Register
  deriving (Eq, Show)

data RunState = RunState {programCounter :: Int, registers :: Array Register Int}
  deriving (Eq, Show)

numberOfEggs = 7

main = do
  input <- Text.lines <$> IO.getContents
  let instructionList = map parseInput input
  let instructions = listArray (0, length instructionList - 1) instructionList
  let finalState = execute initialState instructions
  print finalState

parseInput :: Text -> Instruction
parseInput text = either (error . show) id $ parse parser "" text
  where
    parser = try cpy <|> try inc <|> try dec <|> try jnz <|> try tgl
    cpy = do
      string "cpy "
      from <- value
      string " "
      to <- register
      return $ Copy from to
    inc = Increment <$> (string "inc " >> register)
    dec = Decrement <$> (string "dec " >> register)
    jnz = do
      string "jnz "
      condition <- value
      string " "
      distance <- value
      return $ Jump condition distance
    tgl = do
      string "tgl "
      distance <- value
      return $ Toggle distance
    value = (Direct <$> try number) <|> (Indirect <$> try register)
    register =
      try (string "a" >> return A)
        <|> try (string "b" >> return B)
        <|> try (string "c" >> return C)
        <|> try (string "d" >> return D)
    number = read <$> (many1 digit <|> ((:) <$> char '-' <*> many1 digit))

initialState :: RunState
initialState = RunState 0 (listArray (minBound, maxBound) (numberOfEggs : repeat 0))

execute :: RunState -> Instructions -> RunState
execute state@(RunState counter registers) instructions =
  if counter >= length instructions
    then state
    else execute' (instructions ! counter)
  where
    execute' (Copy (Direct value) destination) =
      execute (nextState {registers = registers // [(destination, value)]}) instructions
    execute' (Copy (Indirect source) destination) =
      execute' (Copy (Direct (registers ! source)) destination)
    execute' (Increment register) =
      execute (nextState {registers = registers // [(register, (registers ! register) + 1)]}) instructions
    execute' (Decrement register) =
      execute (nextState {registers = registers // [(register, (registers ! register) - 1)]}) instructions
    execute' (Jump (Direct 0) distance) =
      execute nextState instructions
    execute' (Jump (Direct _) (Direct distance)) =
      execute (nextState {programCounter = counter + distance}) instructions
    execute' (Jump (Indirect source) distance) =
      execute' (Jump (Direct (registers ! source)) distance)
    execute' (Jump condition (Indirect source)) =
      execute' (Jump condition (Direct (registers ! source)))
    execute' (Toggle (Direct distance)) =
      execute nextState (toggleInstruction (counter + distance) instructions)
    execute' (Toggle (Indirect source)) =
      execute' (Toggle (Direct (registers ! source)))

    nextState = state {programCounter = counter + 1}

toggleInstruction :: Int -> Instructions -> Instructions
toggleInstruction index instructions
  | Ix.inRange (bounds instructions) index = instructions // [(index, toggled (instructions ! index))]
  | otherwise = instructions
  where
    toggled instruction@(Toggle (Direct value)) = InvalidToggle instruction 1
    toggled (Increment register) = Decrement register
    toggled (Decrement register) = Increment register
    toggled (Copy value destination) = Jump value (Indirect destination)
    toggled (Jump condition (Indirect source)) = Copy condition source
    toggled instruction@(Jump _ _) = InvalidToggle instruction 1
