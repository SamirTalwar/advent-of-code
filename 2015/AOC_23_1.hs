import           Data.Array
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import           Text.Parsec
import           Text.Parsec.Text

data Instruction =
    Half Register
  | Triple Register
  | Increment Register
  | Jump Int
  | JumpIfEven Register Int
  | JumpIfOne Register Int
  deriving (Eq, Show)

data Register = A | B
  deriving (Eq, Ord, Enum, Bounded, Ix, Show)

data RunState = RunState { programCounter :: Int, registers :: Array Register Int }
  deriving (Eq, Show)

main = do
  input <- Text.lines <$> IO.getContents
  let instructions = map parseInput input
  let finalState = execute initialState instructions
  print finalState

parseInput :: Text -> Instruction
parseInput text = either (error . show) id $ parse parser "" text
  where
  parser = try hlf <|> try tpl <|> try inc <|> try jmp <|> try jie <|> try jio
  hlf = Half <$> (string "hlf " >> register)
  tpl = Triple <$> (string "tpl " >> register)
  inc = Increment <$> (string "inc " >> register)
  jmp = Jump <$> (string "jmp " >> number)
  jie = do
    string "jie "
    condition <- register
    string ", "
    amount <- number
    return $ JumpIfEven condition amount
  jio = do
    string "jio "
    condition <- register
    string ", "
    amount <- number
    return $ JumpIfOne condition amount
  register = try (string "a" >> return A) <|> try (string "b" >> return B)
  number = read <$> ((optional (char '+') >> many1 digit) <|> ((:) <$> char '-' <*> many1 digit))

initialState :: RunState
initialState = RunState 0 (listArray (minBound, maxBound) (repeat 0))

execute :: RunState -> [Instruction] -> RunState
execute state@(RunState counter registers) instructions =
  if counter >= length instructions
    then state
    else execute' (instructions !! counter)
  where
  execute' (Half register) =
    execute (nextState { registers = registers // [(register, (registers ! register) `div` 2)] }) instructions
  execute' (Triple register) =
    execute (nextState { registers = registers // [(register, (registers ! register) * 3)] }) instructions
  execute' (Increment register) =
    execute (nextState { registers = registers // [(register, (registers ! register) + 1)] }) instructions
  execute' (Jump amount) =
    execute (state { programCounter = counter + amount }) instructions
  execute' (JumpIfEven register amount)
    | even (registers ! register) =
      execute (state { programCounter = counter + amount }) instructions
    | otherwise =
      execute nextState instructions
  execute' (JumpIfOne register amount)
    | registers ! register == 1 =
      execute (state { programCounter = counter + amount }) instructions
    | otherwise =
      execute nextState instructions

  nextState = state { programCounter = counter + 1 }
