module Polyform.Batteries
  ( error
  , Dual
  , Error
  , Errors(..)
  , invalid
  , Msg
  , Validator
  ) where

import Prelude
import Data.Array (singleton) as Array
import Data.Lazy (Lazy) as Data
import Data.Lazy (defer)
import Data.Validation.Semigroup (V)
import Data.Validation.Semigroup (invalid) as Validation
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Polyform.Validator (Validator) as Polyform
import Polyform.Validator.Dual (Dual) as Polyform.Validator.Dual
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol, SProxy)

type Msg
  = Data.Lazy String

-- | It seems that better usability is with this embeded default
-- | error info than with a set of printers which an end user
-- | has to compose with their own renderers.
-- |
-- | Maybe this pattern is more general in the I18N context
-- | and this should be renamed to `Msg` and moved outside?
type Error err
  = { msg ∷ Msg, info ∷ Variant err }

-- | Do we want to migrate to this kind of error repr
--   = Array ({ msg ∷ String, info ∷ Variant errs })
type Errors err
  = Array (Error err)

type Validator m errs i o
  = Polyform.Validator m (Errors errs) i o

type Dual m errs i o
  = Polyform.Validator.Dual.Dual m (Errors errs) i o

-- | Handy shortcuts to quickly build an error or the whole failure result
error ∷ ∀ e errs l t. Row.Cons l e t errs ⇒ IsSymbol l ⇒ SProxy l → (e → String) → e → Errors errs
error l msg = Array.singleton <<< \e → { msg: defer \_ → msg e, info: Variant.inj l e }

invalid ∷ ∀ e errs l o t. Row.Cons l e t errs ⇒ IsSymbol l ⇒ SProxy l → (e → String) → e → V (Errors errs) o
invalid l msg = Validation.invalid <<< error l msg
