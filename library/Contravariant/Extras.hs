module Contravariant.Extras where

import BasePrelude
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import qualified Contravariant.Extras.TH as TH
import qualified TupleTH


contrazip2 :: Divisible f => f a1 -> f a2 -> f ( a1 , a2 )
contrazip2 f1 f2 =
  divide id f1 f2

contrazip3 :: Divisible f => f a1 -> f a2 -> f a3 -> f ( a1 , a2 , a3 )
contrazip3 f1 f2 f3 =
  divide $(TupleTH.splitTupleAt 3 1) f1 $
  contrazip2 f2 f3

contrazip4 :: Divisible f => f a1 -> f a2 -> f a3 -> f a4 -> f ( a1 , a2 , a3 , a4 )
contrazip4 f1 f2 f3 f4 =
  divide $(TupleTH.splitTupleAt 4 1) f1 $
  contrazip3 f2 f3 f4

