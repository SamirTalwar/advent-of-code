{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}

import Data.Functor (void)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe qualified as Maybe
import Helpers.Parse

newtype Thing = Thing String
  deriving newtype (Eq, Ord, Show)

newtype Id = Id Int
  deriving newtype (Eq, Ord, Enum, Show)

newtype Range = Range Int
  deriving newtype (Eq, Show)

type Seeds = Map Id Range

type Mapping = Map Id (Id, Range)

type Mappings = [Mapping]

main :: IO ()
main = do
  (seeds, mappings) <- parseInput parser
  let result = List.find (isSeed seeds . findSeedNumber mappings) [Id 0 ..]
  print $ Maybe.fromJust result

findSeedNumber :: Mappings -> Id -> Id
findSeedNumber mappings location =
  foldr
    ( \mapping destination@(Id destinationValue) ->
        case Map.lookupLE destination mapping of
          Nothing -> destination
          Just (Id destinationStart, (Id sourceStart, Range range)) ->
            let destinationDiff = destinationValue - destinationStart
             in if destinationDiff < range
                  then Id (sourceStart + destinationDiff)
                  else destination
    )
    location
    mappings

isSeed :: Seeds -> Id -> Bool
isSeed seeds seedToCheck@(Id seedToCheckValue) =
  case Map.lookupLE seedToCheck seeds of
    Nothing -> False
    Just (Id seedStart, Range seedRange) -> seedToCheckValue - seedStart <= seedRange

parser :: Parser (Seeds, Mappings)
parser = do
  void $ string "seeds: "
  seeds <-
    Map.fromList
      <$> flip sepBy spaces do
        seedStart <- Id <$> decimal
        spaces
        seedRange <- Range <$> decimal
        pure (seedStart, seedRange)
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
            pure (destinationStart, (sourceStart, range))
        )
  pure (seeds, mappings)
