import Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.List as List
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as IO

main = do
  salt <- Text.strip <$> IO.getContents
  let keys = valid $ hashes salt
  mapM_ print $ take 64 keys

valid :: [Digest MD5] -> [((Int, String), (Int, String))]
valid keys = valid' $ zip [0 ..] (map show keys)
  where
    valid' :: [(Int, String)] -> [((Int, String), (Int, String))]
    valid' (x@(index, key) : xs) =
      case match of
        Just something -> something : valid' xs
        Nothing -> valid' xs
      where
        match = do
          firstSearch <- List.find (\w -> any (== w) (map fst searches)) (windows 3 key)
          let (Just nextSearch) = lookup firstSearch searches
          ((,) x) <$> List.find (List.isInfixOf nextSearch . snd) nextKeys
        nextKeys = take 1000 xs

searches :: [(String, String)]
searches = map (\c -> (replicate 3 c, replicate 5 c)) "0123456789abcdef"

hashes :: Text -> [Digest MD5]
hashes salt = map md5 (salts salt)

salts :: Text -> [Text]
salts salt = map ((salt `Text.append`) . Text.pack . show) [0 ..]

md5 :: Text -> Digest MD5
md5 = hash . encodeUtf8

windows :: Int -> [a] -> [[a]]
windows n = takeWhile (\window -> length window == n) . List.transpose . take n . List.tails
