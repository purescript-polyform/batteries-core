module Polyform.Batteries.UrlEncoded.Types where

import Prelude
import Data.Array (singleton) as Array
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Polyform (Validator) as Polyform
import Polyform.Batteries (Validator, Dual) as Batteries
import Polyform.Dual (hoistParser) as Dual
import Polyform.Validator (lmapValidator)
import Polyform.Validator.Dual (Dual) as Validator.Dual
import Type.Prelude (SProxy(..))

type Name
  = String

type Errors (errs ∷ # Type)
  = Array { name ∷ Name, errors ∷ Array (Variant errs) }

type Validator m (errs ∷ # Type) i o
  = Polyform.Validator m (Errors errs) i o

_urlEncoded = SProxy ∷ SProxy "urlEncoded"

fromValidator ∷ ∀ errs m i. Monad m ⇒ String → Batteries.Validator m errs i ~> Validator m errs i
fromValidator name = lmapValidator (Array.singleton <<< { name, errors: _ })

namespaceValidator ∷ ∀ err i m r. Monad m ⇒ Validator m err i ~> Batteries.Validator m ( urlEncoded ∷ Errors err | r ) i
namespaceValidator = lmapValidator (Array.singleton <<< Variant.inj _urlEncoded)

type Dual m (errs ∷ # Type) i o
  = Validator.Dual.Dual m (Errors errs) i o

fromDual ∷ ∀ errs i m. Monad m ⇒ String → Batteries.Dual m errs i ~> Dual m errs i
fromDual name = Dual.hoistParser (fromValidator name)

namespaceDual ∷ ∀ errs i m. Monad m ⇒ Dual m errs i ~> Batteries.Dual m ( urlEncoded ∷ Errors errs ) i
namespaceDual = Dual.hoistParser namespaceValidator
