{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (mapM_)
import qualified Data.Char as Char
import Data.Text (Text)
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
move MoveUp '3' = '1'
move MoveUp '4' = '4'
move MoveUp '5' = '5'
move MoveUp '6' = '2'
move MoveUp '7' = '3'
move MoveUp '8' = '4'
move MoveUp '9' = '9'
move MoveUp 'A' = '6'
move MoveUp 'B' = '7'
move MoveUp 'C' = '8'
move MoveUp 'D' = 'B'
move MoveRight '1' = '1'
move MoveRight '2' = '3'
move MoveRight '3' = '4'
move MoveRight '4' = '4'
move MoveRight '5' = '6'
move MoveRight '6' = '7'
move MoveRight '7' = '8'
move MoveRight '8' = '9'
move MoveRight '9' = '9'
move MoveRight 'A' = 'B'
move MoveRight 'B' = 'C'
move MoveRight 'C' = 'C'
move MoveRight 'D' = 'D'
move MoveDown '1' = '3'
move MoveDown '2' = '6'
move MoveDown '3' = '7'
move MoveDown '4' = '8'
move MoveDown '5' = '5'
move MoveDown '6' = 'A'
move MoveDown '7' = 'B'
move MoveDown '8' = 'C'
move MoveDown '9' = '9'
move MoveDown 'A' = 'A'
move MoveDown 'B' = 'D'
move MoveDown 'C' = 'C'
move MoveDown 'D' = 'D'
move MoveLeft '1' = '1'
move MoveLeft '2' = '2'
move MoveLeft '3' = '2'
move MoveLeft '4' = '3'
move MoveLeft '5' = '5'
move MoveLeft '6' = '5'
move MoveLeft '7' = '6'
move MoveLeft '8' = '7'
move MoveLeft '9' = '8'
move MoveLeft 'A' = 'A'
move MoveLeft 'B' = 'A'
move MoveLeft 'C' = 'B'
move MoveLeft 'D' = 'D'
