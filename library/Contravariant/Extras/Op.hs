-- |
-- This module exports functions optimized for the `Op` type.
module Contravariant.Extras.Op where

import BasePrelude
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import qualified Contravariant.Extras.TH as TH


-- Generate the `tuple` functions:
return (join (map (TH.opTupleDecs "tuple") (reverse [2..42])))

