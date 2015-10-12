module Contravariant.Extras where

import BasePrelude
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import qualified Contravariant.Extras.TH as TH
import qualified TupleTH


contratuple2 :: Divisible f => f a1 -> f a2 -> f ( a1 , a2 )
contratuple2 f1 f2 =
  divide id f1 f2

contratuple3 :: Divisible f => f a1 -> f a2 -> f a3 -> f ( a1 , a2 , a3 )
contratuple3 f1 f2 f3 =
  divide $(TupleTH.splitTupleAt 3 1) f1 $
  contratuple2 f2 f3

contratuple4 :: Divisible f => f a1 -> f a2 -> f a3 -> f a4 -> f ( a1 , a2 , a3 , a4 )
contratuple4 f1 f2 f3 f4 =
  divide $(TupleTH.splitTupleAt 4 1) f1 $
  contratuple3 f2 f3 f4

