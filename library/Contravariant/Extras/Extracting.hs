module Contravariant.Extras.Extracting where

import BasePrelude
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible


-- |
-- A wrapper, which lifts a contravariant to a covariant using an extractor function.
data Extracting f a =
  forall x. Extracting !(f x) !(x -> a)

instance Functor (Extracting f) where
  {-# INLINE fmap #-}
  fmap f (Extracting contrafunctor extractor) =
    Extracting contrafunctor (fmap f extractor)

instance Divisible f => Applicative (Extracting f) where
  {-# INLINE pure #-}
  pure x =
    Extracting conquer (const x)
  {-# INLINABLE (<*>) #-}
  (<*>) (Extracting contrafunctor1 extractor1) (Extracting contrafunctor2 extractor2) =
    Extracting
    (divide id contrafunctor1 contrafunctor2)
    (\(a1, a2) -> extractor1 a1 (extractor2 a2))

instance Decidable f => Alternative (Extracting f) where
  {-# INLINE empty #-}
  empty =
    Extracting conquer id
  {-# INLINABLE (<|>) #-}
  (<|>) (Extracting contrafunctor1 extractor1) (Extracting contrafunctor2 extractor2) =
    Extracting contrafunctor3 extractor3
    where
      contrafunctor3 =
        choose id contrafunctor1 contrafunctor2
      extractor3 =
        either extractor1 extractor2
