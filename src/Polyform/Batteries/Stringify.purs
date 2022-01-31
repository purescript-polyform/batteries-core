module Polyform.Batteries.Stringify where

import Prelude

import Data.Lazy (force) as Lazy
import Polyform (Validator) as Polyform
import Polyform.Batteries (Dual', Validator') as Batteries
import Polyform.Validator (lmapValidator)
import Polyform.Validator.Dual (Dual) as Polyform.Validator.Dual
import Polyform.Validator.Dual (lmapDual)

type Errors
  = Array String

type Validator m i o
  = Polyform.Validator m Errors i o

type Dual m i o
  = Polyform.Validator.Dual.Dual m Errors i o

stringifyValidator ∷ ∀ m errs i o. Monad m ⇒ Batteries.Validator' m errs i o → Validator m i o
stringifyValidator = lmapValidator (map (Lazy.force <<< _.msg))

stringifyDual ∷ ∀ m errs i o. Monad m ⇒ Batteries.Dual' m errs i o → Dual m i o
stringifyDual = lmapDual (map (Lazy.force <<< _.msg))
