{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import qualified Numeric

main = do
  strings <- Text.lines <$> IO.getContents
  let stringLiterals = map encode strings
  let sizeOfStringLiterals = sum $ map Text.length stringLiterals
  let sizeOfStrings = sum $ map Text.length strings
  print sizeOfStringLiterals
  print sizeOfStrings
  print $ sizeOfStringLiterals - sizeOfStrings

encode :: Text -> Text
encode string = Text.concat ["\"", Text.concatMap encodeChar string, "\""]
  where
    encodeChar '\\' = "\\\\"
    encodeChar '"' = "\\\""
    encodeChar char = Text.singleton char
