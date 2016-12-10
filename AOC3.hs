{-# LANGUAGE OverloadedStrings #-}

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO

data Triangle = Triangle Int Int Int
  deriving (Eq, Show)

instance Read Triangle where
  readsPrec prec input = [(Triangle a b c, cs) |
                           (a, as) <- readsPrec prec input,
                           (b, bs) <- readsPrec prec as,
                           (c, cs) <- readsPrec prec bs]

main = do
  contents <- IO.getContents
  let triangles = map (read . Text.unpack) $ Text.splitOn "\n" $ Text.strip contents
  let possibleTriangles = filter isPossible triangles
  print $ length possibleTriangles

isPossible (Triangle a b c) = a + b > c && a + c > b && b + c > a
