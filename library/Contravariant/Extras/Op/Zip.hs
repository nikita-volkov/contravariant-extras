-- |
-- A berserk collection of @zip@ functions with arities of up to 42.
-- 
-- Why 42?
-- Naturally because it's the answer to the ultimate question of life,
-- the universe and everything.
-- 
-- It's exported as a separate module from "Contravariant.Extras.Op"
-- only to not pollute its documentation.
-- The "Contravariant.Extras.Op" module still reexports this module,
-- so you can simply import that only.
-- 
module Contravariant.Extras.Op.Zip where

import BasePrelude
import qualified Contravariant.Extras.TH as TH


-- Generate the @zip@ functions:
return (join (map (TH.opTupleDecs "tuple") (reverse [2..42])))

