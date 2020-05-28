module Polyform.Batteries.Number where

import Prelude

import Data.Maybe (Maybe, maybe)
import Data.Number (fromString) as Number
import Data.Number.Format (Format, toString, toStringWith) as Number.Format
import Polyform.Batteries (Validator, Dual, error) as Batteries
import Polyform.Dual (dual) as Dual
import Polyform.Validator (liftFnMaybe) as Validator
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

_numberExpected = SProxy ∷ SProxy "numberExpected"

type NumberExpected e = (numberExpected ∷ String | e)

validator ∷ ∀ e m. Monad m ⇒ Batteries.Validator m (NumberExpected + e) String Number
validator = Validator.liftFnMaybe (Batteries.error _numberExpected) Number.fromString

dual ∷ ∀ e m s. Monad m ⇒ Applicative s ⇒ Maybe Number.Format.Format → Batteries.Dual m s (NumberExpected + e) String Number
dual format = Dual.dual
  validator
  (pure <<< maybe Number.Format.toString Number.Format.toStringWith format)
