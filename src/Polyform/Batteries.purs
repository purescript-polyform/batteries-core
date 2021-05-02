module Polyform.Batteries
  ( error
  , Dual
  , Errors(..)
  , invalid
  , Msg
  , msg
  , msg'
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

-- | It seems that better usability is with this embeded default
-- | error info than with a set of printers which an end user
-- | has to compose with their own renderers.
-- |
-- | Maybe this pattern is more general in the I18N context
-- | and this should be renamed to `Msg` and moved outside?
type Msg msg
  = { msg ∷ Data.Lazy String, info ∷ Variant msg }

msg ∷ ∀ info l msg_ msg. IsSymbol l ⇒ Row.Cons l info msg_ msg ⇒ Data.Lazy String → SProxy l → info → Msg msg
msg m l i = { msg: m, info: Variant.inj l i }

msg' ::
  ∀ t6 t7 t8 t9.
  IsSymbol t8 ⇒
  Row.Cons t8 t9 t7 t6 ⇒
  String →
  SProxy t8 →
  t9 →
  { info ∷ Variant t6
  , msg ∷ Data.Lazy String
  }
msg' m = msg (defer \_ → m)

-- | Do we want to migrate to this kind of error repr
--   = Array ({ msg ∷ String, info ∷ Variant errs })
type Errors err
  = Array (Msg err)

type Validator m errs i o
  = Polyform.Validator m (Errors errs) i o

type Dual m errs i o
  = Polyform.Validator.Dual.Dual m (Errors errs) i o

-- | Handy shortcuts to quickly build an error or the whole failure result
error ∷ ∀ e errs l t. Row.Cons l e t errs ⇒ IsSymbol l ⇒ SProxy l → (e → String) → e → Errors errs
error l prt = Array.singleton <<< \e → msg (defer \_ → prt e) l e

invalid ∷ ∀ e errs l o t. Row.Cons l e t errs ⇒ IsSymbol l ⇒ SProxy l → (e → String) → e → V (Errors errs) o
invalid l prt = Validation.invalid <<< error l prt
