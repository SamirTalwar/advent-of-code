module Helpers.Function where

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
