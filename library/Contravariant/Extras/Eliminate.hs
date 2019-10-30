module Contravariant.Extras.Eliminate where

import BasePrelude
import Contravariant.Extras.Contrazip
import Data.Functor.Contravariant.Divisible
import qualified Contravariant.Extras.TH as TH


eliminate2 :: Divisible f => (forall x. g x -> f x) -> g a1 -> g a2 -> f (a1, a2)
eliminate2 fn a b = contrazip2 (fn a) (fn b)

eliminate3 :: Divisible f => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> f (a1, a2, a3)
eliminate3 fn a b c = contrazip3 (fn a) (fn b) (fn c)

eliminate4 :: Divisible f => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> g a4 -> f (a1, a2, a3, a4)
eliminate4 fn a b c d = contrazip4 (fn a) (fn b) (fn c) (fn d)

eliminate5 :: Divisible f => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> g a4 -> g a5 -> f (a1, a2, a3, a4, a5)
eliminate5 fn a b c d e = contrazip5 (fn a) (fn b) (fn c) (fn d) (fn e)

eliminate6 :: Divisible f => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> g a4 -> g a5 -> g a6 -> f (a1, a2, a3, a4, a5, a6)
eliminate6 fn a b c d e f = contrazip6 (fn a) (fn b) (fn c) (fn d) (fn e) (fn f)

eliminate7 :: Divisible f => (forall x. g x -> f x) -> g a1 -> g a2 -> g a3 -> g a4 -> g a5 -> g a6 -> g a7 -> f (a1, a2, a3, a4, a5, a6, a7)
eliminate7 fn a b c d e f g = contrazip7 (fn a) (fn b) (fn c) (fn d) (fn e) (fn f) (fn g)
