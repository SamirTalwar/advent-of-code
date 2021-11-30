data Triangle = Triangle Int Int Int
  deriving (Eq, Show)

main = do
  contents <- getContents
  let triangles = readTriangles $ init contents
  let possibleTriangles = filter isPossible triangles
  print $ length possibleTriangles

readTriangles "" = []
readTriangles input =
  let ([a, b, c], rest) = readNumbers input 3
   in (Triangle a b c) : readTriangles rest
  where
    readNumbers :: String -> Int -> ([Int], String)
    readNumbers string 0 = ([], string)
    readNumbers string n =
      let [(number, rest)] = readsPrec 0 string
          (numbers, restOfString) = readNumbers rest (n - 1)
       in ((number : numbers), restOfString)

isPossible (Triangle a b c) = a + b > c && a + c > b && b + c > a
