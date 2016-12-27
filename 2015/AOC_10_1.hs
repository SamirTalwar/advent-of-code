import qualified Data.Char as Char

main = do
  input <- trim <$> getContents
  let iterations = iterate lookAndSay input
  print $ length $ iterations !! 40

trim :: String -> String
trim = takeWhile (not . Char.isSpace) . dropWhile Char.isSpace

lookAndSay :: String -> String
lookAndSay "" = ""
lookAndSay string@(n : _) = show (length match) ++ (n : lookAndSay rest)
  where
  (match, rest) = span (== n) string
