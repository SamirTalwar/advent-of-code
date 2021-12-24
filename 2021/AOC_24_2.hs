{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

import Control.Monad (foldM)
import Data.Bool (bool)
import Data.Functor
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as Vector
import Helpers.Parse
import System.Environment (getArgs)
import Text.Parsec hiding (Error, State)
import Prelude hiding (lookup)

modelNumberLength :: Int
modelNumberLength = 14

data Var = W | X | Y | Z
  deriving (Show)

data Value = Var Var | Num Int

instance Show Value where
  show (Var var) = show var
  show (Num num) = show num

data State = State Int Int Int Int
  deriving (Show)

data Instruction
  = Inp Var
  | Add Var Value
  | Mul Var Value
  | Div Var Value
  | Mod Var Value
  | Eql Var Value
  deriving (Show)

type Program = [Instruction]

type Input = Vector Int

data Error = DivisionByZero | NegativeModulus
  deriving (Show)

data Result a = Done a | NoInputLeft State | Error Error
  deriving (Functor, Show)

instance Applicative Result where
  pure = Done
  Error failure <*> _ = Error failure
  _ <*> Error failure = Error failure
  NoInputLeft state <*> _ = NoInputLeft state
  _ <*> NoInputLeft state = NoInputLeft state
  Done f <*> Done x = Done (f x)

instance Monad Result where
  Error failure >>= _ = Error failure
  NoInputLeft state >>= _ = NoInputLeft state
  Done x >>= f = f x

main :: IO ()
main = do
  program <- parseLinesIO parser
  argsInput <- Vector.fromList . map read <$> getArgs
  if null argsInput
    then putStrLn $ concatMap show $ Vector.toList $ findInput program
    else print $ run program argsInput

findInput :: Program -> Input
findInput program =
  let input = findInput' program (Vector.replicate modelNumberLength Nothing)
   in correctInput program input

findInput' :: Program -> Vector (Maybe Int) -> Input
findInput' program decided =
  let input = Vector.catMaybes decided
   in if length input == modelNumberLength
        then input
        else
          let attempts = do
                position <- filter (\n -> Maybe.isNothing (decided ! n)) [0 .. modelNumberLength - 1]
                value <- [1 .. 9]
                let updated = decided // [(position, Just value)]
                let trial = Vector.map (Maybe.fromMaybe 1) updated
                return (updated, run program trial)
           in findInput' program $ selectBest attempts

correctInput :: Program -> Input -> Input
correctInput program input =
  case run program input of
    Done (State _ _ _ 0) -> input
    _ ->
      let attempts = do
            position <- [0 .. modelNumberLength - 1]
            value <- [1 .. 9]
            let trial = input // [(position, value)]
            return (trial, run program trial)
       in correctInput program $ selectBest attempts

selectBest :: [(a, Result State)] -> a
selectBest results =
  fst $
    List.minimumBy (comparing (\(_, State _ _ _ z) -> z)) $
      results >>= \case (updated, Done state) -> [(updated, state)]; _ -> []

run :: Program -> Input -> Result State
run program input = snd <$> foldM eval (input, State 0 0 0 0) program

eval :: (Input, State) -> Instruction -> Result (Input, State)
eval (input, state) (Inp a) =
  case Vector.uncons input of
    Nothing -> NoInputLeft state
    Just (i, rest) -> Done (rest, update a i state)
eval (input, state) (Add a b) =
  let c = lookup (Var a) state + lookup b state
   in Done (input, update a c state)
eval (input, state) (Mul a b) =
  let c = lookup (Var a) state * lookup b state
   in Done (input, update a c state)
eval (input, state) (Div a b) =
  case lookup b state of
    0 -> Error DivisionByZero
    b' ->
      let c = lookup (Var a) state `quot` b'
       in Done (input, update a c state)
eval (input, state) (Mod a b) =
  case (lookup (Var a) state, lookup b state) of
    (_, 0) -> Error DivisionByZero
    (a', _) | a' < 0 -> Error NegativeModulus
    (_, b') | b' < 0 -> Error NegativeModulus
    (a', b') ->
      let c = a' `mod` b'
       in Done (input, update a c state)
eval (input, state) (Eql a b) =
  let c = bool 0 1 $ lookup (Var a) state == lookup b state
   in Done (input, update a c state)

update :: Var -> Int -> State -> State
update W w (State _ x y z) = State w x y z
update X x (State w _ y z) = State w x y z
update Y y (State w x _ z) = State w x y z
update Z z (State w x y _) = State w x y z

lookup :: Value -> State -> Int
lookup (Var W) (State w _ _ _) = w
lookup (Var X) (State _ x _ _) = x
lookup (Var Y) (State _ _ y _) = y
lookup (Var Z) (State _ _ _ z) = z
lookup (Num n) _ = n

parser :: Parsec Text () Instruction
parser =
  choice
    [ try (string "inp") *> (Inp <$> var),
      try (string "add") *> (Add <$> var <*> value),
      try (string "mul") *> (Mul <$> var <*> value),
      try (string "div") *> (Div <$> var <*> value),
      try (string "mod") *> (Mod <$> var <*> value),
      try (string "eql") *> (Eql <$> var <*> value)
    ]
  where
    value = try (Var <$> var) <|> (Num <$> (spaces *> int))
    var =
      spaces
        *> choice
          [ char 'w' $> W,
            char 'x' $> X,
            char 'y' $> Y,
            char 'z' $> Z
          ]
