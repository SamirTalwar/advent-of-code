{-# OPTIONS -Wall #-}

import qualified Data.List as List
import qualified Data.Maybe as Maybe

main :: IO ()
main = do
  code <- lines <$> getContents
  let scores = map score $ Maybe.mapMaybe complete code
  let middle = length scores `div` 2
  let answer = List.sort scores !! middle
  print answer

score :: [Char] -> Int
score = foldl (\s c -> s * 5 + score' c) 0
  where
    score' ')' = 1
    score' ']' = 2
    score' '}' = 3
    score' '>' = 4
    score' _ = error "Invalid character."

complete :: [Char] -> Maybe [Char]
complete line = complete' line []
  where
    complete' "" stack = Just $ map pair stack
    complete' (')' : cs) ('(' : stack) = complete' cs stack
    complete' (')' : _) _ = Nothing
    complete' (']' : cs) ('[' : stack) = complete' cs stack
    complete' (']' : _) _ = Nothing
    complete' ('}' : cs) ('{' : stack) = complete' cs stack
    complete' ('}' : _) _ = Nothing
    complete' ('>' : cs) ('<' : stack) = complete' cs stack
    complete' ('>' : _) _ = Nothing
    complete' (c : cs) stack = complete' cs (c : stack)

pair :: Char -> Char
pair '(' = ')'
pair '[' = ']'
pair '{' = '}'
pair '<' = '>'
pair _ = error "Invalid character."
