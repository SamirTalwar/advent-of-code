{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

import Data.Functor (void)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Helpers.Parse

newtype Thing = Thing String
  deriving newtype (Eq, Ord, Show)

newtype Id = Id Int
  deriving newtype (Eq, Ord, Show)

newtype Range = Range Int
  deriving newtype (Eq, Show)

type Seeds = [Id]

type Mapping = Map Id (Id, Range)

type Mappings = [Mapping]

main :: IO ()
main = do
  (seeds, mappings) <- parseInput parser
  let locations = foldl (\input mapping -> map (lookupIn mapping) input) seeds mappings
      result = minimum locations
  print result

lookupIn :: Mapping -> Id -> Id
lookupIn mapping source@(Id sourceValue) =
  case Map.lookupLE source mapping of
    Nothing -> source
    Just (Id sourceStart, (Id destinationStart, Range range)) ->
      let sourceDiff = sourceValue - sourceStart
       in if sourceDiff < range
            then Id (destinationStart + sourceDiff)
            else source

parser :: Parser (Seeds, Mappings)
parser = do
  void $ string "seeds: "
  seeds <- map Id . List.sort <$> decimal `sepBy` spaces
  void $ some newline
  mappings <-
    many $
      between
        (skipManyTill anySingle (void newline))
        (void newline <|> eof)
        ( Map.fromList <$> some do
            destinationStart <- Id <$> decimal
            spaces
            sourceStart <- Id <$> decimal
            spaces
            range <- Range <$> decimal
            void newline
            pure (sourceStart, (destinationStart, range))
        )
  pure (seeds, mappings)
