module Polyform.Batteries
  ( error
  , Dual
  , Dual'
  , Errors(..)
  , Errors'
  , invalid
  , onErr
  , Msg
  , msg
  , msg'
  , Validator
  , Validator'
  ) where

import Prelude

import Data.Array (singleton) as Array
import Data.Lazy (Lazy) as Data
import Data.Lazy (defer)
import Data.Symbol (reflectSymbol)
import Data.Validation.Semigroup (V)
import Data.Validation.Semigroup (invalid) as Validation
import Data.Variant (Variant)
import Data.Variant (inj) as Variant
import Data.Variant.Internal (VariantRep(..))
import Polyform.Validator (Validator) as Polyform
import Polyform.Validator.Dual (Dual) as Polyform.Validator.Dual
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

-- | It seems that better usability is with this embeded default
-- | error info than with a set of printers which an end user
-- | has to compose with their own renderers.
-- |
-- | Maybe this pattern is more general in the I18N context
-- | and this should be renamed to `Msg` and moved outside?
type Msg info
  = { msg ∷ Data.Lazy String, info ∷ Variant info }

msg ∷ ∀ info l infos_ infos. IsSymbol l ⇒ Row.Cons l info infos_ infos ⇒ Data.Lazy String → Proxy l → info → Msg infos
msg m l i = { msg: m, info: Variant.inj l i }

msg' ::
  ∀ t6 t7 t8 t9.
  IsSymbol t8 ⇒
  Row.Cons t8 t9 t7 t6 ⇒
  String →
  Proxy t8 →
  t9 →
  { info ∷ Variant t6
  , msg ∷ Data.Lazy String
  }
msg' m = msg (defer \_ → m)

-- | Do we want to migrate to this kind of error repr
--   = Array ({ msg ∷ String, info ∷ Variant errs })
type Errors err
  = Array err

-- | Usually we use this representation so we carry
-- | semantic error information around.
type Errors' err = Errors (Msg err)

type Validator m errs i o = Polyform.Validator m (Errors errs) i o

type Validator' m (errs :: Row Type) i o = Validator m (Msg errs) i o

type Dual m errs i o = Polyform.Validator.Dual.Dual m (Errors errs) i o

type Dual' m (errs :: Row Type) i o = Dual m (Msg errs) i o

-- | Handy shortcuts to quickly build an error or the whole failure result
error ∷ ∀ e errs l t. Row.Cons l e t errs ⇒ IsSymbol l ⇒ Proxy l → (e → String) → e → Errors' errs
error l prt = Array.singleton <<< \e → msg (defer \_ → prt e) l e

invalid ∷ ∀ e errs l o t. Row.Cons l e t errs ⇒ IsSymbol l ⇒ Proxy l → (e → String) → e → V (Errors' errs) o
invalid l prt = Validation.invalid <<< error l prt

-- | Similar to `Variant.on` but on `Msg`.
-- | You can 
-- |
-- | ```
-- | flattenEnumErr = do
-- |   let
-- |     reMsg = msg' "Invalid choice" _invalidEnumValue
-- |   identity
-- |     # onErrInfo _invalidEnumIndex (reMsg <<< show)
-- |     # onErrInfo _intExpected reMsg
-- | ```

onErr
  ∷ ∀ proxy sym info b infos_ infos
  . Row.Cons sym info infos_ infos
  ⇒ IsSymbol sym
  ⇒ proxy sym
  → ({ info :: info, msg :: Data.Lazy String } → b)
  → (Msg infos_ → b)
  → Msg infos
  → b
onErr p f g { msg: m, info: r } =
  case coerceV r of
    VariantRep v | v.type == reflectSymbol p → f { info: v.value, msg: m }
    _ → g { msg: m, info: coerceR r }
  where
  coerceV ∷ Variant infos → VariantRep info
  coerceV = unsafeCoerce

  coerceR ∷ Variant infos → Variant infos_
  coerceR = unsafeCoerce


