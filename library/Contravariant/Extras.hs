module Contravariant.Extras
(
  -- |
  -- A berserk collection of @contrazip@ functions with arities of up to 42.
  module Contravariant.Extras.Contrazip,
  module Contravariant.Extras.Eliminate,
  (>*<),
  contramany,
  Supplied(..),
)
where

import BasePrelude hiding ((<>))
import Contravariant.Extras.Contrazip
import Contravariant.Extras.Eliminate
import Data.Functor.Contravariant.Divisible
import Data.Semigroup (Semigroup ((<>)))


-- |
-- An alias to 'divided'.
{-# INLINE (>*<) #-}
(>*<) :: Divisible f => f a -> f b -> f (a, b)
(>*<) =
  divided

contramany :: Decidable f => f a -> f [a]
contramany f =
  loop
  where
    loop =
      choose chooser cons nil
      where
        chooser =
          \case
            head : tail ->
              Left (head, tail)
            _ ->
              Right ()
        cons =
          divide id f loop
        nil =
          conquer

-- |
-- A combination of a divisible functor with some input for it.
-- Allows to use the 'Monoid' API for composition.
data Supplied divisible =
  forall input. Supplied !(divisible input) !input

instance Divisible divisible => Semigroup (Supplied divisible) where
  Supplied divisible1 input1 <> Supplied divisible2 input2 =
    Supplied divisible3 input3
    where
      divisible3 =
        divide id divisible1 divisible2
      input3 =
        (input1, input2)

instance Divisible divisible => Monoid (Supplied divisible) where
  mempty =
    Supplied conquer ()
  mappend =
    (<>)
