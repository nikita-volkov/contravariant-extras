module Contravariant.Extras where

import BasePrelude
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import qualified Language.Haskell.TH as TH


{-# INLINE premap #-}
premap :: Contravariant f => (a -> b) -> f b -> f a
premap =
  contramap


-- Generate functions like the following:
-- 
-- @
-- tuple3 :: Monoid a => Op a b1 -> Op a b2 -> Op a b3 -> Op a ( b1 , b2 , b3 )
-- tuple3 (Op op1) (Op op2) (Op op3) =
--   Op $ \(v1, v2, v3) -> op1 v1 <> op2 v2 <> op3 v3
-- @
let
  decs arity =
    [ signature , value ]
    where
      signature =
        undefined
      value =
        undefined
  in
    return (join (map decs [2..7]))

