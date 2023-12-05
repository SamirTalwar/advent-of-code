module Helpers.Parse
  ( module Text.Megaparsec,
    module Text.Megaparsec.Char,
    Lexer.decimal,
    Parser,
    parseInput,
    spaces,
    word,
  )
where

import Data.Functor (void)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lexer

type Parser = Text.Megaparsec.Parsec Void String

parseInput :: Parser a -> IO a
parseInput parser = do
  contents <- getContents
  either (fail . Text.Megaparsec.errorBundlePretty) pure $
    Text.Megaparsec.parse (parser <* eof) "input" contents

spaces :: Parser ()
spaces = void $ some (char ' ')

word :: Parser String
word = many letterChar
