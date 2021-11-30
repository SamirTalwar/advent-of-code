import Data.Foldable (toList)
import qualified Data.List as List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text

type HappinessTransfer = ((Person, Person), Int)

type HappinessTransfers = Map (Person, Person) Int

newtype Person = Person String
  deriving (Eq, Ord, Show)

me = Person "me"

main = do
  happinessTransfersWithoutMe <- Map.fromList <$> map parseInput <$> Text.lines <$> IO.getContents
  let peopleWithoutMe = peopleIn happinessTransfersWithoutMe
  let happinessTransfers = includingMe peopleWithoutMe happinessTransfersWithoutMe
  let people = me : peopleWithoutMe
  let peoplePermutations = map pairs $ List.permutations people
  let happinessTransfersPermutations = solve happinessTransfers peoplePermutations
  print $ maximum $ map sum happinessTransfersPermutations

parseInput :: Text -> HappinessTransfer
parseInput text = either (error . show) id $ parse parser "" text
  where
    parser = do
      who <- person
      string " would "
      transfer <- happinessTransfer
      string " by sitting next to "
      whom <- person
      string "."
      return ((who, whom), transfer)
    happinessTransfer = try gain <|> try lose
    gain = do
      string "gain "
      value <- number
      string " happiness units"
      return value
    lose = do
      string "lose "
      value <- number
      string " happiness units"
      return (- value)
    person = Person <$> many1 letter
    number = read <$> many1 digit

peopleIn :: HappinessTransfers -> [Person]
peopleIn = toList . Set.fromList . concatMap (\(to, from) -> [to, from]) . Map.keys

includingMe :: [Person] -> HappinessTransfers -> HappinessTransfers
includingMe people = Map.union $ Map.fromList (concatMap (\person -> [((me, person), 0), ((person, me), 0)]) people)

solve :: HappinessTransfers -> [[(Person, Person)]] -> [[Int]]
solve happinessTransfers = map (map (\(a, b) -> happinessTransfers ! (a, b) + happinessTransfers ! (b, a)))

pairs :: [a] -> [(a, a)]
pairs list = (last list, head list) : pairs' list
  where
    pairs' [] = []
    pairs' [x] = []
    pairs' (a : xs@(b : _)) = (a, b) : pairs' xs
