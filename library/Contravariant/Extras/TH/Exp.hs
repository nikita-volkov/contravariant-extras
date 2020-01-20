module Contravariant.Extras.TH.Exp (
  contrazipLambda,
) where

import Contravariant.Extras.Prelude
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Language.Haskell.TH.Syntax hiding (classP)
import qualified TemplateHaskell.Compat.V0208 as Compat


{-|
Contrazip lambda expression of specified arity.

Allows to create contrazip expressions of any arity:

>>>:t $(return (contrazipLambda 2))
$(return (contrazipLambda 2))
  :: Data.Functor.Contravariant.Divisible.Divisible f =>
     f b -> f c -> f (b, c)
-}
contrazipLambda :: Int -> Exp
contrazipLambda arity = LamE pats body where
  pats = map pat (enumFromTo 1 arity) where
    pat index = VarP name where
      name = mkName (showString "f" (show index))
  body = exp arity where
    exp index = case index of
      1 -> VarE (mkName (showString "f" (show arity)))
      _ -> foldl1 AppE [
          VarE 'divide
          ,
          splitTupleAt index 1
          ,
          VarE (mkName (showString "f" (show (arity - index + 1))))
          ,
          exp (pred index)
        ]

splitTupleAt :: Int -> Int -> Exp
splitTupleAt arity position =
  let
    nameByIndex index = Name (OccName ('_' : show index)) NameS
    names = enumFromTo 0 (pred arity) & map nameByIndex
    pats = names & map VarP
    pat = TupP pats
    exps = names & map VarE
    body = splitAt position exps & \ (a, b) -> Compat.tupE [Compat.tupE a, Compat.tupE b]
    in LamE [pat] body
