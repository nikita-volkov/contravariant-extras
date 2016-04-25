module Contravariant.Extras
(
  -- |
  -- A berserk collection of @contrazip@ functions with arities of up to 42.
  module Contravariant.Extras.Contrazip,
  contramany,
)
where

import BasePrelude
import Contravariant.Extras.Contrazip
import Data.Functor.Contravariant.Divisible


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

