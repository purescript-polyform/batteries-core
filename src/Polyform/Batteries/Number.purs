module Polyform.Batteries.Number where

import Prelude
import Data.Maybe (Maybe, maybe)
import Data.Number (fromString) as Number
import Data.Number.Format (Format, toString, toStringWith) as Number.Format
import Polyform.Batteries (Validator, Dual, error) as Batteries
import Polyform.Dual (dual) as Dual
import Polyform.Validator (liftFnMaybe) as Validator
import Type.Row (type (+))
import Type.Proxy (Proxy(..))

_numberExpected = Proxy ∷ Proxy "numberExpected"

type NumberExpected e
  = ( numberExpected ∷ String | e )

-- | TODO: Move these two pieces `Number.Formatter`
-- | module and use `purescript-formatters`
-- | API there.
validator ∷ ∀ e m. Monad m ⇒ Batteries.Validator m (NumberExpected + e) String Number
validator = Validator.liftFnMaybe (Batteries.error _numberExpected $ append "Expecting a number but got: ") Number.fromString

dual ∷ ∀ e m. Monad m ⇒ Maybe Number.Format.Format → Batteries.Dual m (NumberExpected + e) String Number
dual format =
  Dual.dual
    validator
    (pure <<< maybe Number.Format.toString Number.Format.toStringWith format)
