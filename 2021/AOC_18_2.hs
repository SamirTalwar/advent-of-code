{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveFunctor #-}

import Data.Bifunctor
import Data.Text (Text)
import Helpers.List
import Helpers.Parse
import Text.Parsec

data Number = Value Int | Number :+ Number

instance Show Number where
  show (Value n) = show n
  show (left :+ right) = "[" <> show left <> ", " <> show right <> "]"

data Explosion = Explosion (Maybe Int) Number (Maybe Int)
  deriving (Show)

data ExplosionDirection = L | R

data Progress a b = Restart a | Next b
  deriving (Functor)

instance Applicative (Progress a) where
  pure = Next
  Restart f <*> _ = Restart f
  Next _ <*> Restart x = Restart x
  Next f <*> Next x = Next (f x)

instance Monad (Progress a) where
  Restart x >>= _ = Restart x
  Next x >>= f = f x

instance Bifunctor Progress where
  bimap f _ (Restart x) = Restart (f x)
  bimap _ g (Next x) = Next (g x)

both :: Bifunctor f => (a -> b) -> f a a -> f b b
both func = bimap func func

progress :: (a -> c) -> (b -> c) -> Progress a b -> c
progress f _ (Restart x) = f x
progress _ g (Next x) = g x

main :: IO ()
main = do
  numbers <- parseLinesIO parser
  let selections = selectPairs numbers
  let results = map (magnitude . uncurry add) selections
  print $ maximum results

magnitude :: Number -> Int
magnitude (Value n) = n
magnitude (left :+ right) = 3 * magnitude left + 2 * magnitude right

add :: Number -> Number -> Number
add a b = reduce $ a :+ b

reduce :: Number -> Number
reduce = progress (reduce . consumeExplosion) (progress reduce id . split) . explode 0
  where
    consumeExplosion (Explosion _ value _) = value

explode :: Int -> Number -> Progress Explosion Number
explode _ number@Value {} = Next number
explode 4 (Value left :+ Value right) =
  Restart $ Explosion (Just left) (Value 0) (Just right)
explode depth (left :+ right) = do
  left' <- first (propagateExplosion L right) (explode (succ depth) left)
  right' <- first (propagateExplosion R left') (explode (succ depth) right)
  return $ left' :+ right'
  where
    propagateExplosion L r (Explosion incL l incR) = Explosion incL (l :+ propagateExplosion' L incR r) Nothing
    propagateExplosion R l (Explosion incL r incR) = Explosion Nothing (propagateExplosion' R incL l :+ r) incR
    propagateExplosion' _ Nothing number = number
    propagateExplosion' _ (Just inc) (Value n) = Value (n + inc)
    propagateExplosion' L inc (l :+ r) = propagateExplosion' L inc l :+ r
    propagateExplosion' R inc (l :+ r) = l :+ propagateExplosion' R inc r

split :: Number -> Progress Number Number
split number@(Value n)
  | n >= 10 =
    let (d, r) = n `divMod` 2
     in Restart $ Value d :+ Value (d + r)
  | otherwise =
    Next number
split (left :+ right) = do
  left' <- first (:+ right) (split left)
  both (left' :+) (split right)

parser :: Parsec Text () Number
parser = do
  _ <- char '['
  left <- Value <$> try int <|> try parser
  _ <- char ','
  right <- Value <$> try int <|> try parser
  _ <- char ']'
  return $ left :+ right
