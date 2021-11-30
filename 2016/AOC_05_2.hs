{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (mapM_)
import qualified Crypto.Hash
import Data.ByteString (ByteString)
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.IO as IO
import qualified Numeric

main = do
  doorId <- Text.strip <$> IO.getContents
  IO.putStrLn $ crackPassword doorId

crackPassword doorId =
  [0 ..]
    |> map (\i -> hashText (doorId `Text.append` (Text.pack $ show i)))
    |> filter (\digest -> Text.take 5 digest == "00000")
    |> map (\digest -> (fst $ head $ Numeric.readHex [Text.index digest 5], Text.index digest 6))
    |> assemble 8
    |> Text.pack

assemble n characters =
  map (\position -> snd $ Maybe.fromJust $ List.find (\(p, _) -> position == p) characters) [0 .. (n - 1)]

hashText :: Text.Text -> Text.Text
hashText text = Text.pack $ show $ md5 $ encodeUtf8 text

md5 :: ByteString -> Crypto.Hash.Digest Crypto.Hash.MD5
md5 = Crypto.Hash.hash

(|>) = flip ($)
