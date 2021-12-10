{-# OPTIONS -Wall #-}

main :: IO ()
main = do
  code <- lines <$> getContents
  print $ sum $ map score code

score :: [Char] -> Int
score line = score' line []
  where
    score' "" _ = 0
    score' (')' : cs) ('(' : stack) = score' cs stack
    score' (')' : _) _ = 3
    score' (']' : cs) ('[' : stack) = score' cs stack
    score' (']' : _) _ = 57
    score' ('}' : cs) ('{' : stack) = score' cs stack
    score' ('}' : _) _ = 1197
    score' ('>' : cs) ('<' : stack) = score' cs stack
    score' ('>' : _) _ = 25137
    score' (c : cs) stack = score' cs (c : stack)
