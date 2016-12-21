import           Data.Foldable (toList)
import qualified Data.List as List
import           Data.Sequence as Seq
import           Data.Sequence (Seq)
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import           Text.Parsec
import           Text.Parsec.Text

data Operation =
    SwapPositions Position Position
  | SwapLetters Letter Letter
  | RotateDirection Direction Int
  | RotatePosition Letter
  | ReversePositions Position Position
  | MovePosition Position Position
  deriving (Eq, Show)
newtype Position = Position Int
  deriving (Eq, Show)
newtype Letter = Letter Char
  deriving (Eq, Show)
data Direction = DLeft | DRight
  deriving (Eq, Show)

plaintext = "abcdefgh"

main = do
  input <- Text.lines <$> IO.getContents
  let operations = map parseInput input
  let cyphertexts = map toList $ List.scanl' (flip apply) (Seq.fromList plaintext) operations
  mapM_ putStrLn cyphertexts

parseInput :: Text -> Operation
parseInput text = either (error . show) id $ parse parser "" text
  where
  parser = firstOf [swapPositions, swapLetters, rotateDirection, rotatePosition, reversePositions, movePosition]
  swapPositions = do
    string "swap "
    x <- position
    string " with "
    y <- position
    return $ SwapPositions x y
  swapLetters = do
    string "swap "
    x <- letter
    string " with "
    y <- letter
    return $ SwapLetters x y
  rotateDirection = do
    string "rotate "
    d <- direction
    space
    steps <- number
    string " step"
    optional (char 's')
    return $ RotateDirection d steps
  rotatePosition = do
    string "rotate based on position of "
    l <- letter
    return $ RotatePosition l
  reversePositions = do
    string "reverse positions "
    x <- number
    string " through "
    y <- number
    return $ ReversePositions (Position x) (Position y)
  movePosition = do
    string "move "
    x <- position
    string " to "
    y <- position
    return $ MovePosition x y
  position = Position <$> (string "position " >> number)
  letter = Letter <$> (string "letter " >> anyChar)
  direction = (string "left" >> return DLeft) <|> (string "right" >> return DRight)
  number = read <$> many1 digit
  firstOf = List.foldl1' (<|>) . map try

apply :: Operation -> Seq Char -> Seq Char
apply (SwapPositions (Position x) (Position y)) plaintext =
  case (plaintext !? x, plaintext !? y) of
    (Just a, Just b) -> update x b $ update y a plaintext
    _ ->  error $ "Could not find both the positions " ++ show x ++ " and " ++ show y ++ " in \"" ++ toList plaintext ++ "\"."
apply (SwapLetters (Letter a) (Letter b)) plaintext =
  case (elemIndexL a plaintext, elemIndexL b plaintext) of
    (Just x, Just y) -> apply (SwapPositions (Position x) (Position y)) plaintext
    _ -> error $ "Could not find both the letters '" ++ [a] ++ "' and '" ++ [b] ++ "' in \"" ++ toList plaintext ++ "\"."
apply (RotateDirection DLeft n) plaintext =
  let (left, right) = Seq.splitAt n plaintext
  in right >< left
apply (RotateDirection DRight n) plaintext =
  apply (RotateDirection DLeft (Seq.length plaintext - n)) plaintext
apply (RotatePosition (Letter a)) plaintext =
  case elemIndexL a plaintext of
    Just x -> apply (RotateDirection DRight ((1 + x + if x >= 4 then 1 else 0) `mod` Seq.length plaintext)) plaintext
    _ ->  error $ "Could not find the letter " ++ [a] ++ " in \"" ++ toList plaintext ++ "\"."
apply (ReversePositions (Position x) (Position y)) plaintext =
  let (left, rest) = Seq.splitAt x plaintext
      (middle, right) = Seq.splitAt (y - x + 1) rest
  in left >< Seq.reverse middle >< right
apply (MovePosition (Position x) (Position y)) plaintext =
  case plaintext !? x of
    Just a -> insertAt y a $ deleteAt x plaintext
    _ ->  error $ "Could not find the position " ++ show x ++ " in \"" ++ toList plaintext ++ "\"."
