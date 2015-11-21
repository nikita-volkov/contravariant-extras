-- |
-- A berserk collection of @contramap@ functions with arities of up to 42.
-- 
-- Why 42?
-- Well, naturally, because it's the answer to the ultimate question of life,
-- the universe and everything.
-- 
-- It's exported as a separate module from "Contravariant.Extras"
-- only to not pollute its documentation.
-- The "Contravariant.Extras" module still reexports this module,
-- so you can simply import that only.
-- 
module Contravariant.Extras.Contramap where

import BasePrelude
import qualified Contravariant.Extras.TH as TH


-- Generate the @contramap@ functions:
return (join (map (TH.divisibleContramapDecs "contramap") (reverse [2..42])))

