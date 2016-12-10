{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (mapM_)
import qualified Data.Char as Char
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO

data Movement = MoveUp | MoveRight | MoveDown | MoveLeft
  deriving (Eq, Show)

type Key = Char

main = do
  contents <- IO.getContents
  let movements = decode contents
  putStrLn $ tail $ scanl solve '5' movements

decode :: Text -> [[Movement]]
decode = map (map decode' . Text.unpack) . Text.splitOn "\n" . Text.strip
  where
  decode' 'U' = MoveUp
  decode' 'R' = MoveRight
  decode' 'D' = MoveDown
  decode' 'L' = MoveLeft

solve :: Key -> [Movement] -> Key
solve = foldl $ flip move

move :: Movement -> Key -> Key

move MoveUp '1' = '1'
move MoveUp '2' = '2'
move MoveUp '3' = '3'
move MoveUp '4' = '1'
move MoveUp '5' = '2'
move MoveUp '6' = '3'
move MoveUp '7' = '4'
move MoveUp '8' = '5'
move MoveUp '9' = '6'

move MoveRight '1' = '2'
move MoveRight '2' = '3'
move MoveRight '3' = '3'
move MoveRight '4' = '5'
move MoveRight '5' = '6'
move MoveRight '6' = '6'
move MoveRight '7' = '8'
move MoveRight '8' = '9'
move MoveRight '9' = '9'

move MoveDown '1' = '4'
move MoveDown '2' = '5'
move MoveDown '3' = '6'
move MoveDown '4' = '7'
move MoveDown '5' = '8'
move MoveDown '6' = '9'
move MoveDown '7' = '7'
move MoveDown '8' = '8'
move MoveDown '9' = '9'

move MoveLeft '1' = '1'
move MoveLeft '2' = '1'
move MoveLeft '3' = '2'
move MoveLeft '4' = '4'
move MoveLeft '5' = '4'
move MoveLeft '6' = '5'
move MoveLeft '7' = '7'
move MoveLeft '8' = '7'
move MoveLeft '9' = '8'
