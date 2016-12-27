{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy as ByteString
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as Map
import qualified Data.Scientific as Scientific

main = do
  (Just accounts) <- decode <$> ByteString.getContents
  let numbers = filterNumbers accounts
  print $ sum numbers

filterNumbers :: Value -> [Int]
filterNumbers (Bool _) = []
filterNumbers (String _) = []
filterNumbers (Number scientific) =
  let (Right integer) = Scientific.floatingOrInteger scientific
  in [integer]
filterNumbers (Array vector) = concatMap filterNumbers $ toList vector
filterNumbers (Object hashMap)
  | (String "red") `elem` Map.elems hashMap = []
  | otherwise = concatMap filterNumbers $ Map.elems hashMap
