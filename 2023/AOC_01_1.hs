import Data.Char qualified as Char

main :: IO ()
main = do
  inputLines <- lines <$> getContents
  let numbers :: [Int] = map (read . (\xs -> [head xs, last xs]) . filter Char.isDigit) inputLines
  let result = sum numbers
  print result
