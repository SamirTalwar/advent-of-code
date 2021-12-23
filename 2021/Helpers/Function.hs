module Helpers.Function where

infixl 0 |>

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

infixl 9 .>

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = flip (.)
