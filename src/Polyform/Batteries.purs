module Polyform.Batteries
  ( error
  , Dual
  , Errors(..)
  , invalid
  , Validator
  ) where

import Prelude

import Data.Array (singleton) as Array
import Data.Validation.Semigroup (V)
import Data.Validation.Semigroup (invalid) as Validation
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Polyform.Validator (Validator) as Polyform
import Polyform.Validator.Dual (Dual) as Polyform.Validator.Dual
import Polyform.Validator (lmapValidator)
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol, SProxy(..))

type Errors errs = Array (Variant errs)

type Validator m errs i o = Polyform.Validator m (Errors errs) i o

type Dual m errs i o = Polyform.Validator.Dual.Dual m (Errors errs) i o

-- | Handy shortcuts to quickly build an error or the whole failure result
error ∷ ∀ e errs l t. Row.Cons l e t errs ⇒ IsSymbol l ⇒ SProxy l → e → Errors errs
error l = Array.singleton <<< Variant.inj l

invalid ∷ ∀ e errs l o t. Row.Cons l e t errs ⇒ IsSymbol l ⇒ SProxy l → e → V (Errors errs) o
invalid l = Validation.invalid <<< error l

_polyform = SProxy ∷ SProxy "polyform"

namespaceValidator ∷ ∀ err i o m r. Monad m ⇒ Validator m err i o → Validator m (polyform ∷ Variant err | r) i o
namespaceValidator = lmapValidator (map (Variant.inj _polyform))


