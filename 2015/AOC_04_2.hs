{-# LANGUAGE OverloadedStrings #-}

import qualified Crypto.Hash
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as IO

main = do
  input <- Text.strip <$> IO.getContents
  print $ mineAdventCoins input

mineAdventCoins :: Text -> (Int, Text)
mineAdventCoins input =
  head $
    filter (\(i, digest) -> Text.take 6 digest == "000000") $
      map (\i -> (i, hashText $ Text.append input $ Text.pack $ show i)) $
        [1 ..]

hashText :: Text -> Text
hashText text = Text.pack $ show $ md5 $ encodeUtf8 text

md5 :: ByteString -> Crypto.Hash.Digest Crypto.Hash.MD5
md5 = Crypto.Hash.hash
