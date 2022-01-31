module Polyform.Batteries.Generic.Enum where

import Prelude

import Data.Enum (class BoundedEnum, fromEnum, toEnum)
import Data.Maybe (Maybe)
import Polyform.Batteries (Dual', Validator')
import Polyform.Batteries (error) as Batteries
import Polyform.Dual (dual) as Dual
import Polyform.Validator (liftFnMaybe) as Validator
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

_invalidEnumIndex = Proxy ∷ Proxy "invalidEnumIndex"

type InvalidEnumIndex e
  = ( invalidEnumIndex ∷ Int | e )

-- | We use a `Proxy` value here to help the compiler choose
-- | the output enum type.
-- | If the output type is clear in your given context you can
-- | use `validator'` and skip this `Proxy a` altogether.
validator ∷ ∀ a err m. Applicative m ⇒ BoundedEnum a ⇒ Proxy a → Validator' m (InvalidEnumIndex + err) Int a
validator _ = Validator.liftFnMaybe (Batteries.error _invalidEnumIndex (append "Invalid enum index: " <<< show)) (toEnum ∷ Int → Maybe a)

-- | If you have clearly typed output passing in a `Proxy a` is redundant.
validator' ∷ ∀ a err m. Applicative m ⇒ BoundedEnum a ⇒ Validator' m (InvalidEnumIndex + err) Int a
validator' = validator (Proxy ∷ Proxy a)

dual ∷ ∀ a err m. Applicative m ⇒ BoundedEnum a ⇒ Proxy a → Dual' m (InvalidEnumIndex + err) Int a
dual p = Dual.dual (validator p) (pure <<< fromEnum)

dual' ∷ ∀ a err m. Applicative m ⇒ BoundedEnum a ⇒ Dual' m (InvalidEnumIndex + err) Int a
dual' = dual (Proxy ∷ Proxy a)
