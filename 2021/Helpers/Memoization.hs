{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- This is basically copied from the `MemoTrie` package, for learning. -}
{- https://hackage.haskell.org/package/MemoTrie -}

module Helpers.Memoization
  ( memo,
    memo2,
    memo3,
    memo4,
    HasTrie (..),
  )
where

import Data.Bool (bool)
import Data.Set (Set)
import qualified Data.Set as Set
import Helpers.Numbers

memo :: HasTrie a => (a -> b) -> a -> b
memo = unTrie . trie

memo2 :: (HasTrie a, HasTrie b) => (a -> b -> c) -> a -> b -> c
memo2 = curry . memo . uncurry

memo3 :: (HasTrie a, HasTrie b, HasTrie c) => (a -> b -> c -> d) -> a -> b -> c -> d
memo3 = curry3 . memo . uncurry3

memo4 :: (HasTrie a, HasTrie b, HasTrie c, HasTrie d) => (a -> b -> c -> d -> e) -> a -> b -> c -> d -> e
memo4 = curry4 . memo . uncurry4

curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e
curry4 f a b c d = f (a, b, c, d)

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

class HasTrie a where
  infixr 2 :->:
  data (:->:) a :: * -> *
  trie :: (a -> b) -> a :->: b
  unTrie :: (a :->: b) -> a -> b

instance HasTrie Bool where
  data Bool :->: x = BoolTrie x x
  trie f = BoolTrie (f False) (f True)
  unTrie (BoolTrie ifFalse ifTrue) = bool ifFalse ifTrue

instance HasTrie a => HasTrie (Maybe a) where
  data Maybe a :->: x = MaybeTrie x (a :->: x)
  trie f = MaybeTrie (f Nothing) (trie (f . Just))
  unTrie (MaybeTrie ifNothing ifJust) = maybe ifNothing (unTrie ifJust)

instance (HasTrie a, HasTrie b) => HasTrie (Either a b) where
  data Either a b :->: x = EitherTrie (a :->: x) (b :->: x)
  trie f = EitherTrie (trie (f . Left)) (trie (f . Right))
  unTrie (EitherTrie ifLeft ifRight) = either (unTrie ifLeft) (unTrie ifRight)

instance (HasTrie a, HasTrie b) => HasTrie (a, b) where
  data (a, b) :->: x = Tuple2Trie (a :->: b :->: x)
  trie f = Tuple2Trie $ trie $ \a -> trie $ \b -> f (a, b)
  unTrie (Tuple2Trie f) (a, b) = unTrie (unTrie f a) b

instance (HasTrie a, HasTrie b, HasTrie c) => HasTrie (a, b, c) where
  data (a, b, c) :->: x = Tuple3Trie (a :->: b :->: c :->: x)
  trie f = Tuple3Trie $ trie $ \a -> trie $ \b -> trie $ \c -> f (a, b, c)
  unTrie (Tuple3Trie f) (a, b, c) = unTrie (unTrie (unTrie f a) b) c

instance (HasTrie a, HasTrie b, HasTrie c, HasTrie d) => HasTrie (a, b, c, d) where
  data (a, b, c, d) :->: x = Tuple4Trie (a :->: b :->: c :->: d :->: x)
  trie f = Tuple4Trie $ trie $ \a -> trie $ \b -> trie $ \c -> trie $ \d -> f (a, b, c, d)
  unTrie (Tuple4Trie f) (a, b, c, d) = unTrie (unTrie (unTrie (unTrie f a) b) c) d

instance HasTrie a => HasTrie [a] where
  data [a] :->: x = ListTrie x (a :->: [a] :->: x)
  trie f = ListTrie (f []) (trie $ \x -> trie $ \xs -> f (x : xs))
  unTrie (ListTrie ifNil _) [] = ifNil
  unTrie (ListTrie _ ifCons) (x : xs) = unTrie (unTrie ifCons x) xs

instance (Ord a, HasTrie a) => HasTrie (Set a) where
  data Set a :->: x = SetTrie ([a] :->: x)
  trie f = SetTrie $ trie (f . Set.fromList)
  unTrie (SetTrie f) = unTrie f . Set.toList

instance HasTrie Int where
  data Int :->: x = IntTrie ([Bool] :->: x)
  trie f = IntTrie $ trie $ f . unBits
  unTrie (IntTrie f) = unTrie f . bits
