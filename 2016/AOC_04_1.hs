import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as IO
import Text.Parsec
import Text.Parsec.Text

data Room = Room {roomName :: Name, roomSectorId :: SectorId, roomChecksum :: Checksum}
  deriving (Eq, Show)

type Name = String

type SectorId = Int

type Checksum = String

main = do
  lines <- Text.lines <$> IO.getContents
  let rooms = map parseRoom lines
  let realRooms = filter realRoom rooms
  print $ sum $ map roomSectorId realRooms

parseRoom :: Text -> Room
parseRoom text = case parse parser "" text of
  Right room -> room
  where
    parser = do
      name <- many1 (letter <|> char '-')
      sectorId <- read <$> many1 digit
      char '['
      checksum <- many1 letter
      char ']'
      return $ Room name sectorId checksum

realRoom (Room name _ checksum) =
  let nameLetters = map snd $ List.sortBy nameOrder $ map (\cs -> (length cs, head cs)) $ List.group $ List.sort $ filter (/= '-') name
   in take (length checksum) nameLetters == checksum

nameOrder (an, ac) (bn, bc) =
  case compare an bn of
    LT -> GT
    GT -> LT
    EQ -> compare ac bc
