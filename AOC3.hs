data Triangle = Triangle Int Int Int
  deriving (Eq, Show)

main = do
  contents <- getContents
  let triangles = readTriangles $ init contents
  let possibleTriangles = filter isPossible triangles
  print $ length possibleTriangles

readTriangles "" = []
readTriangles input =
  let ([aa, ba, ca, ab, bb, cb, ac, bc, cc], rest) = readNumbers input 9
  in [Triangle aa ab ac, Triangle ba bb bc, Triangle ca cb cc] ++ readTriangles rest
  where
  readNumbers :: String -> Int -> ([Int], String)
  readNumbers string 0 = ([], string)
  readNumbers string n =
    let [(number, rest)] = readsPrec 0 string
        (numbers, restOfString) = readNumbers rest (n - 1)
    in ((number : numbers), restOfString)

isPossible (Triangle a b c) = a + b > c && a + c > b && b + c > a
