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

data Node = Node String Char
  deriving stock (Eq, Ord, Show)

main :: IO ()
main = do
  (instructions, network) <- parseInput parser
  let instructionCount = length instructions
      starts = filter isStart $ Map.keys network
      loops = map (findLoopSize network instructions) starts
      result = product (map (`div` instructionCount) loops) * instructionCount
  print result

isStart :: Node -> Bool
isStart (Node _ 'A') = True
isStart _ = False

-- This makes a couple of assumptions about the data.
--
-- Note that I'm defining "chunk" as a set of "moves" the length of the instruction set.
--
-- The data is as follows:
-- 1. there is an initial chunk, exactly the length of the instructions
-- 2. then there are a number of chunks before the finishing node
-- 3. the finishing node is always at the end of a chunk
-- 4. after reaching the finishing node, it loops back to the second chunk
--
-- We therefore just need to know the loop size, and subtract the "offset".
findLoopSize :: Network -> Instructions -> Node -> Int
findLoopSize network instructions start = findLoop' [] start 0 mempty
  where
    findLoop' :: Instructions -> Node -> Int -> Map Node Int -> Int
    findLoop' [] node size seen =
      case Map.lookup node seen of
        Just offset -> size - offset
        Nothing -> findLoop' instructions node size (Map.insert node size seen)
    findLoop' (instruction : rest) node size seen =
      findLoop' rest (next network instruction node) (size + 1) seen

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
    node = do
      a <- alphaNumChar
      b <- alphaNumChar
      Node [a, b] <$> letterChar
