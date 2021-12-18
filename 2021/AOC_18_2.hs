{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections #-}

import Data.Bifunctor
import qualified Data.List as List
import Data.Text (Text)
import Helpers.Parse
import Text.Parsec

data Number = Value Int | Pair Number Number

instance Show Number where
  show (Value n) = show n
  show (Pair left right) = "[" <> show left <> ", " <> show right <> "]"

data Explosion = Explosion (Maybe Int) Number (Maybe Int)
  deriving (Show)

data ExplosionDirection = L | R

main :: IO ()
main = do
  numbers <- parseLinesIO parser
  let selections = selectPairs numbers
  let results = map (magnitude . uncurry add) selections
  print $ maximum results

selectPairs :: [a] -> [(a, a)]
selectPairs xs = concat $ zipWith (\i x -> let (before, after) = List.splitAt i xs in map (x,) (before ++ tail after)) [0 ..] xs

magnitude :: Number -> Int
magnitude (Value n) = n
magnitude (Pair left right) = 3 * magnitude left + 2 * magnitude right

add :: Number -> Number -> Number
add a b = reduce $ Pair a b

reduce :: Number -> Number
reduce number =
  case explode 0 number of
    Left (Explosion _ result _) -> reduce result
    Right exploded ->
      case split exploded of
        Left result -> reduce result
        Right result -> result

explode :: Int -> Number -> Either Explosion Number
explode _ number@Value {} = Right number
explode depth (Pair left right)
  | depth == 4 =
    case (left, right) of
      (Value l, Value r) -> Left $ Explosion (Just l) (Value 0) (Just r)
      _ -> error $ "Cannot explode " ++ show left ++ " and " ++ show right ++ "."
  | otherwise = do
    case explode (succ depth) left of
      Left (Explosion incL l incR) ->
        let r = propagateExplosion L incR right
         in Left $ Explosion incL (Pair l r) Nothing
      Right l -> do
        case explode (succ depth) right of
          Left (Explosion incL r incR) ->
            let l' = propagateExplosion R incL l
             in Left $ Explosion Nothing (Pair l' r) incR
          Right r -> Right $ Pair l r
  where
    propagateExplosion _ Nothing number = number
    propagateExplosion _ (Just inc) (Value n) = Value (n + inc)
    propagateExplosion L inc (Pair l r) = Pair (propagateExplosion L inc l) r
    propagateExplosion R inc (Pair l r) = Pair l (propagateExplosion R inc r)

split :: Number -> Either Number Number
split number@(Value n)
  | n >= 10 =
    let (d, r) = n `divMod` 2
     in Left $ Pair (Value d) (Value (d + r))
  | otherwise =
    Right number
split (Pair left right) = do
  left' <- first (`Pair` right) $ split left
  right' <- first (Pair left') $ split right
  return $ Pair left' right'

parser :: Parsec Text () Number
parser = do
  _ <- char '['
  left <- (Value <$> try int) <|> try parser
  _ <- char ','
  right <- (Value <$> try int) <|> try parser
  _ <- char ']'
  return $ Pair left right
