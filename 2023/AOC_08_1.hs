{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

import Data.Functor (void, ($>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Helpers.Parse

type Instructions = [Instruction]

data Instruction = GoLeft | GoRight
  deriving stock (Eq, Show)

type Network = Map Node (Node, Node)

newtype Node = Node String
  deriving newtype (Eq, Ord, Show)

main :: IO ()
main = do
  (instructionsOnce, network) <- parseInput parser
  let instructions = cycle instructionsOnce
  let steps = length (follow network instructions start)
  print steps

start :: Node
start = Node "AAA"

finish :: Node
finish = Node "ZZZ"

follow :: Network -> Instructions -> Node -> [Node]
follow _ [] _ = error "Out of instructions."
follow network (instruction : instructions) current
  | current == finish = []
  | otherwise = let subsequent = next network instruction current in subsequent : follow network instructions subsequent

next :: Network -> Instruction -> Node -> Node
next network instruction node =
  case Map.lookup node network of
    Nothing -> error $ "Unknown node: " <> show node
    Just (l, r) -> case instruction of
      GoLeft -> l
      GoRight -> r

parser :: Parser (Instructions, Network)
parser = do
  instructions <- some instruction
  void newline
  void newline
  network <-
    Map.fromList <$> many do
      here <- node
      void $ string " = ("
      left <- node
      void $ string ", "
      right <- node
      void $ string ")"
      void newline
      pure (here, (left, right))
  pure (instructions, network)
  where
    instruction = goLeft <|> goRight
    goLeft = char 'L' $> GoLeft
    goRight = char 'R' $> GoRight
    node = Node <$> some letterChar
