{-# OPTIONS -Wall #-}

module Helpers.Parse where

import Data.Text (Text)
import qualified Data.Text.IO as Text.IO
import Text.Parsec

parseTextIO :: Parsec Text () a -> IO a
parseTextIO parser = parseText parser <$> Text.IO.getContents

parseText :: Parsec Text () a -> Text -> a
parseText parser = either (error . show) id . parse (parser <* eof) ""

parseLinesIO :: Parsec Text () a -> IO [a]
parseLinesIO parser = parseLines parser <$> Text.IO.getContents

parseLines :: Parsec Text () a -> Text -> [a]
parseLines = parseText . linesP

linesP :: Parsec Text () a -> Parsec Text () [a]
linesP parser = many (parser <* optional (string "\n"))

int :: Parsec Text a Int
int = read <$> many1 digit
