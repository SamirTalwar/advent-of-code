{-# OPTIONS -Wall #-}

module Helpers.Parse where

import Data.Text (Text)
import Text.Parsec

parseText :: Parsec Text () a -> Text -> a
parseText parser = either (error . show) id . parse parser ""

int :: Parsec Text a Int
int = read <$> many1 digit
