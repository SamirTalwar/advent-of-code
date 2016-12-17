{-# LANGUAGE ScopedTypeVariables #-}

import           Data.Array
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import           Text.Parsec
import           Text.Parsec.Text

data Instruction =
    StartingValue Carrier Value
  | Give Carrier Carrier Carrier
  deriving (Eq, Show)
data Carrier = Bot Int | Output Int
  deriving (Eq, Show)
data Value = Value Int
  deriving (Eq, Ord, Show)

data BotState = BotState (Maybe Value) (Maybe Value)
  deriving (Eq, Show)
data OutputState = OutputState (Maybe Value)
  deriving (Eq, Show)

data Operation = Gave Carrier (Value, Carrier) (Value, Carrier)
  deriving (Eq, Show)

main = do
  input <- Text.lines <$> IO.getContents
  let instructions = map parseInput input
  let theBotRange = botRange instructions
  let botsWithNothing = listArray theBotRange $ repeat (BotState Nothing Nothing)
  let theOutputRange = outputRange instructions
  let startingOutputs = listArray theOutputRange $ repeat (OutputState Nothing)
  let (startingValues, gives) = splitUp instructions
  let startingBots = disseminate startingValues botsWithNothing
  let operations = run (cycle gives) startingBots startingOutputs
  print $ List.find (\(Gave source (lowValue, _) (highValue, _)) -> lowValue == Value 17 && highValue == Value 61) operations

parseInput :: Text -> Instruction
parseInput text = either (error . show) id $ parse parser "" text
  where
  parser = try startingValue <|> try give
  startingValue = do
    v <- value
    string " goes to "
    c <- carrier
    return $ StartingValue c v
  give = do
    source <- carrier
    string " gives low to "
    lowDestination <- carrier
    string " and high to "
    highDestination <- carrier
    return $ Give source lowDestination highDestination
  value = Value <$> (string "value " >> number)
  carrier = try bot <|> try output
  bot = Bot <$> (string "bot " >> number)
  output = Output <$> (string "output " >> number)
  number = read <$> many1 digit

botRange :: [Instruction] -> (Int, Int)
botRange instructions = (Set.findMin bots, Set.findMax bots)
  where
  bots = Set.fromList $ concatMap identifyBots instructions
  identifyBots (StartingValue (Bot bot) _) = [bot]
  identifyBots (Give (Bot source) (Bot lowDestination) (Bot highDestination)) = [source, lowDestination, highDestination]
  identifyBots (Give (Bot source) (Bot lowDestination) _) = [source, lowDestination]
  identifyBots (Give (Bot source) _ (Bot highDestination)) = [source, highDestination]
  identifyBots (Give _ (Bot lowDestination) (Bot highDestination)) = [lowDestination, highDestination]
  identifyBots (Give (Bot source) _ _) = [source]
  identifyBots (Give _ (Bot lowDestination) _) = [lowDestination]
  identifyBots (Give _ _ (Bot highDestination)) = [highDestination]
  identifyBots _ = []

outputRange :: [Instruction] -> (Int, Int)
outputRange instructions = (Set.findMin outputs, Set.findMax outputs)
  where
  outputs = Set.fromList $ concatMap identifyOutputs instructions
  identifyOutputs (Give _ (Output lowDestination) (Output highDestination)) = [lowDestination, highDestination]
  identifyOutputs (Give _ (Output lowDestination) _) = [lowDestination]
  identifyOutputs (Give _ _ (Output highDestination)) = [highDestination]
  identifyOutputs _ = []

splitUp :: [Instruction] -> ([Instruction], [Instruction])
splitUp instructions = List.partition isStartingValue instructions
  where
  isStartingValue (StartingValue _ _) = True
  isStartingValue _ = False

disseminate :: [Instruction] -> Array Int BotState -> Array Int BotState
disseminate startingValues bots = accum give bots startingValuesByBot
  where
  startingValuesByBot = map (\(StartingValue (Bot bot) value) -> (bot, value)) startingValues

run :: [Instruction] -> Array Int BotState -> Array Int OutputState -> [Operation]
run [] bots outputs = []
run (Give carrier@(Bot bot) lowDestination highDestination : next) bots outputs =
  if all (\(BotState low high) -> Maybe.isNothing low || Maybe.isNothing high) (elems bots)
  then []
  else case bots ! bot of
    BotState (Just valueA) (Just valueB) ->
      let
        lowValue = min valueA valueB
        highValue = max valueA valueB
        newOperation = Gave carrier (lowValue, lowDestination) (highValue, highDestination)
      in case (lowDestination, highDestination) of
        (Bot low, Bot high) ->
          newOperation : run
            next
            (bots // [
              (low, give (bots ! low) lowValue),
              (high, give (bots ! high) highValue),
              (bot, BotState Nothing Nothing)
            ])
            outputs
        (Bot low, Output high) ->
          newOperation : run
            next
            (bots // [
              (low, give (bots ! low) lowValue),
              (bot, BotState Nothing Nothing)
            ])
            (outputs // [
              (high, OutputState (Just highValue))
            ])
        (Output low, Bot high) ->
          newOperation : run
            next
            (bots // [
              (high, give (bots ! high) highValue),
              (bot, BotState Nothing Nothing)
            ])
            (outputs // [
              (low, OutputState (Just lowValue))
            ])
        (Output low, Output high) ->
          newOperation : run
            next
            (bots // [
              (bot, BotState Nothing Nothing)
            ])
            (outputs // [
              (low, OutputState (Just lowValue)),
              (high, OutputState (Just highValue))
            ])
    _ ->
      run next bots outputs

give (BotState Nothing Nothing) value = BotState (Just value) Nothing
give (BotState (Just value1) Nothing) value2 = BotState (Just value1) (Just value2)
give (BotState (Just value1) (Just value2)) value3 =
  error $ "Tried to give a bot " ++ show value3 ++ ", but it already had " ++ show value1 ++ " and " ++ show value2 ++ "."
