import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import           Text.Parsec
import           Text.Parsec.Text

data Ingredient =
  Ingredient {
    name :: String,
    capacity :: Int,
    durability :: Int,
    flavor :: Int,
    texture :: Int,
    calories :: Int
  }
  deriving (Eq, Show)

zeroIngredient = Ingredient "Nothing" 0 0 0 0 0

main = do
  ingredients <- map parseInput <$> Text.lines <$> IO.getContents
  let distributions = distribute 100 (length ingredients)
  let scores = map (\distribution -> score $ zip distribution ingredients) distributions
  print $ maximum scores

parseInput :: Text -> Ingredient
parseInput text = either (error . show) id $ parse parser "" text
  where
  parser = do
    name <- many1 letter
    string ": capacity "
    capacity <- number
    string ", durability "
    durability <- number
    string ", flavor "
    flavor <- number
    string ", texture "
    texture <- number
    string ", calories "
    calories <- number
    return $ Ingredient name capacity durability flavor texture calories
  number = read <$> (many1 digit <|> ((:) <$> char '-' <*> many1 digit))

distribute :: Int -> Int -> [[Int]]
distribute n 0 = [[]]
distribute n length = concatMap (\i -> map (i :) $ distribute (n - i) (length - 1)) [0..n]

score :: [(Int, Ingredient)] -> Int
score ingredients = multiply $ capAtZero $ foldr accumulateScore zeroIngredient ingredients
  where
  multiply (Ingredient _ capacity durability flavor texture _) =
    capacity * durability * flavor * texture
  capAtZero ingredient =
    ingredient {
      capacity = max 0 (capacity ingredient),
      durability = max 0 (durability ingredient),
      flavor = max 0 (flavor ingredient),
      texture = max 0 (texture ingredient)
    }
  accumulateScore (amount, ingredient) accumulator =
    accumulator {
      capacity = capacity accumulator + amount * capacity ingredient,
      durability = durability accumulator + amount * durability ingredient,
      flavor = flavor accumulator + amount * flavor ingredient,
      texture = texture accumulator + amount * texture ingredient
    }
