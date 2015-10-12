module Contravariant.Extras where

import BasePrelude
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import qualified Contravariant.Extras.TH as TH


{-# INLINE premap #-}
premap :: Contravariant f => (a -> b) -> f b -> f a
premap =
  contramap

