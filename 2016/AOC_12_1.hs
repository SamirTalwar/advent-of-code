import Control.Monad (mapM_)
import Data.Array
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text

data Instruction
  = Copy Value Register
  | Increment Register
  | Decrement Register
  | Jump Value Int
  deriving (Eq, Show)

data Register = A | B | C | D
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

data Value
  = Direct Int
  | Indirect Register
  deriving (Eq, Show)

data RunState = RunState {programCounter :: Int, registers :: Array Register Int}
  deriving (Eq, Show)

main = do
  input <- Text.lines <$> IO.getContents
  let instructions = map parseInput input
  let finalState = execute initialState instructions
  print finalState

parseInput :: Text -> Instruction
parseInput text = either (error . show) id $ parse parser "" text
  where
    parser = try cpy <|> try inc <|> try dec <|> try jnz
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
      amount <- number
      return $ Jump condition amount
    value = (Direct <$> try number) <|> (Indirect <$> try register)
    register =
      try (string "a" >> return A)
        <|> try (string "b" >> return B)
        <|> try (string "c" >> return C)
        <|> try (string "d" >> return D)
    number = read <$> (many1 digit <|> ((:) <$> char '-' <*> many1 digit))

initialState :: RunState
initialState = RunState 0 (listArray (minBound, maxBound) (repeat 0))

execute :: RunState -> [Instruction] -> RunState
execute state@(RunState counter registers) instructions =
  if counter >= length instructions
    then state
    else execute' (instructions !! counter)
  where
    execute' (Copy (Direct value) destination) =
      execute (nextState {registers = registers // [(destination, value)]}) instructions
    execute' (Copy (Indirect source) destination) =
      execute' (Copy (Direct (registers ! source)) destination)
    execute' (Increment register) =
      execute (nextState {registers = registers // [(register, (registers ! register) + 1)]}) instructions
    execute' (Decrement register) =
      execute (nextState {registers = registers // [(register, (registers ! register) - 1)]}) instructions
    execute' (Jump (Direct 0) amount) =
      execute nextState instructions
    execute' (Jump (Direct _) amount) =
      execute (nextState {programCounter = counter + amount}) instructions
    execute' (Jump (Indirect source) amount) =
      execute' (Jump (Direct (registers ! source)) amount)

    nextState = state {programCounter = counter + 1}
