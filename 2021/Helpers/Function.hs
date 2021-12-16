module Helpers.Function where

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
