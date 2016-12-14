import qualified Data.List as List
import qualified Data.Ord as Ord

main = do
  messages <- lines <$> getContents
  let characters = List.transpose messages
  let modalCharacters = map modal characters
  putStrLn modalCharacters

modal = head . last . List.sortBy (Ord.comparing length) . List.group . List.sort
