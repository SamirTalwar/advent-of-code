module Helpers.Applicative where

import Control.Applicative

(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
