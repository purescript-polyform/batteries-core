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

type Name = String

type Errors (errs ∷ # Type) = Array { name ∷ Name, errors ∷ Array (Variant errs) }

type Validator m (errs ∷ # Type) i o = Polyform.Validator m (Errors errs) i o

_urlEncoded = SProxy ∷ SProxy "urlEncoded"

fromValidator ∷ ∀ errs m i. Monad m ⇒ String → Batteries.Validator m errs i ~> Validator m errs i
fromValidator name = lmapValidator (Array.singleton <<< { name, errors: _ })

namespaceValidator ∷ ∀ err i m r. Monad m ⇒ Validator m err i ~> Batteries.Validator m (urlEncoded ∷ Errors err | r) i
namespaceValidator = lmapValidator (Array.singleton <<< Variant.inj _urlEncoded)

type Dual m s (errs ∷ # Type) i o = Validator.Dual.Dual m s (Errors errs) i o

fromDual ∷ ∀ errs i m s. Monad m ⇒ String → Batteries.Dual m s errs i ~> Dual m s errs i
fromDual name = Dual.hoistParser (fromValidator name)

namespaceDual ∷ ∀ errs i m s. Monad m ⇒ Dual m s errs i ~> Batteries.Dual m s (urlEncoded ∷ Errors errs) i
namespaceDual = Dual.hoistParser namespaceValidator
