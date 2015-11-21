{-# LANGUAGE CPP #-}
module Contravariant.Extras.TH where

import BasePrelude
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Language.Haskell.TH hiding (classP)
import qualified TupleTH


-- |
-- Generates declarations like the following:
-- 
-- @
-- tuple3 :: Monoid a => Op a b1 -> Op a b2 -> Op a b3 -> Op a ( b1 , b2 , b3 )
-- tuple3 ( Op op1 ) ( Op op2 ) ( Op op3 ) =
--   Op $ \( v1 , v2 , v3 ) -> mconcat [ op1 v1 , op2 v2 , op3 v3 ]
-- @
opContramapDecs :: String -> Int -> [ Dec ]
opContramapDecs baseName arity =
  [ signature , value ]
  where
    name =
      mkName (showString baseName (show arity))
    signature =
      SigD name type_
      where
        type_ =
          ForallT vars cxt type_
          where
            vars =
              map (PlainTV . mkName) ("a" : bs)
              where
                bs =
                  map b (enumFromTo 1 arity)
                  where
                    b index =
                      showString "b" (show index)
            cxt =
              [ pred ]
              where
                pred =
                  classP ''Monoid [ a ]
                  where
                    a =
                      VarT (mkName "a") 
            type_ =
              foldr appArrowT result params
              where
                appArrowT a b =
                  AppT (AppT ArrowT a) b
                a =
                  VarT (mkName "a")
                result =
                  AppT (AppT (ConT ''Op) a) tuple
                  where
                    tuple =
                      foldl AppT (TupleT arity) params
                      where
                        params =
                          map param (enumFromTo 1 arity)
                          where
                            param index =
                              VarT (mkName (showString "b" (show index)))
                params =
                  map param (enumFromTo 1 arity)
                  where
                    param index =
                      AppT (AppT (ConT ''Op) a) b
                      where
                        b =
                          VarT (mkName (showString "b" (show index)))
    value =
      FunD name clauses
      where
        clauses =
          [ clause ]
          where
            clause =
              Clause pats body []
              where
                pats =
                  map pat (enumFromTo 1 arity)
                  where
                    pat index =
                      ConP 'Op pats
                      where
                        pats =
                          [ VarP name ]
                          where
                            name =
                              mkName (showString "op" (show index))
                body =
                  NormalB (AppE (ConE 'Op) lambda)
                  where
                    lambda =
                      LamE pats exp
                      where
                        pats =
                          [ TupP pats ]
                          where
                            pats =
                              map pat (enumFromTo 1 arity)
                              where
                                pat index =
                                  VarP (mkName (showString "v" (show index)))
                        exp =
                          AppE (VarE 'mconcat) (ListE applications)
                          where
                            applications =
                              map application (enumFromTo 1 arity)
                              where
                                application index =
                                  AppE (VarE opName) (VarE varName)
                                  where
                                    opName =
                                      mkName (showString "op" (show index))
                                    varName =
                                      mkName (showString "v" (show index))

-- |
-- Generates declarations in the spirit of the following:
-- 
-- @
-- contramap4 :: Divisible f => f a1 -> f a2 -> f a3 -> f a4 -> f ( a1 , a2 , a3 , a4 )
-- contramap4 f1 f2 f3 f4 =
--   divide $(TupleTH.splitTupleAt 4 1) f1 $
--   divide $(TupleTH.splitTupleAt 3 1) f2 $
--   divide $(TupleTH.splitTupleAt 2 1) f3 $
--   f4
-- @
divisibleContramapDecs :: String -> Int -> [Dec]
divisibleContramapDecs baseName arity =
  [signature, value]
  where
    name =
      mkName (showString baseName (show arity))
    signature =
      SigD name type_
      where
        type_ =
          ForallT vars cxt type_
          where
            fName =
              mkName "f"
            aNames =
              map aName (enumFromTo 1 arity)
              where
                aName index =
                  mkName (showString "a" (show index))
            vars =
              map PlainTV (fName : aNames)
            cxt =
              [pred]
              where
                pred =
                  classP ''Divisible [VarT fName]
            type_ =
              foldr appArrowT result params
              where
                appArrowT a b =
                  AppT (AppT ArrowT a) b
                result =
                  AppT (VarT fName) tuple
                  where
                    tuple =
                      foldl AppT (TupleT arity) (map VarT aNames)
                params =
                  map param aNames
                  where
                    param aName =
                      AppT (VarT fName) (VarT aName)
    value =
      FunD name clauses
      where
        clauses =
          [clause]
          where
            clause =
              Clause pats body []
              where
                pats =
                  map pat (enumFromTo 1 arity)
                  where
                    pat index =
                      VarP name
                      where
                        name =
                          mkName (showString "f" (show index))
                body =
                  NormalB (exp arity)
                  where
                    exp index =
                      case index of
                        1 ->
                          VarE (mkName (showString "f" (show arity)))
                        _ ->
                          foldl1 AppE
                          [
                            VarE 'divide
                            ,
                            splitTupleAtE index 1
                            ,
                            VarE (mkName (showString "f" (show (arity - index + 1))))
                            ,
                            exp (pred index)
                          ]

splitTupleAtE :: Int -> Int -> Exp
splitTupleAtE arity position =
  unsafePerformIO $
  runQ $
  TupleTH.splitTupleAt arity position

classP :: Name -> [Type] -> Pred
#if MIN_VERSION_template_haskell(2,10,0)
classP n tl = foldl AppT (ConT n) tl
#else
classP = ClassP
#endif
