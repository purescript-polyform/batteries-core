module Polyform.Batteries.String.Duals where

import Prelude

import Polyform.Batteries (Dual) as Batteries
import Polyform.Batteries.Generic.Ord.Validators (Range)
import Polyform.Batteries.String.Validators (NotEqualTo, NotGreaterThan, NotInRange, NotMissingFrom, NotOneOf, NotSmallerThan, NotDifferentThan)
import Polyform.Batteries.String.Validators (differentThan, equalTo, greaterThan, inRange, missingFrom, oneOf, smallerThan) as Validators
import Polyform.Dual (dual') as Dual
import Type.Row (type (+))

greaterThan
  ∷ ∀ err m s. Applicative m
  ⇒ Applicative s
  ⇒ String
  → Batteries.Dual m s (NotGreaterThan + err) String String
greaterThan max = Dual.dual' (Validators.greaterThan max)

smallerThan
  ∷ ∀ err m s. Applicative m
  ⇒ Applicative s
  ⇒ String
  → Batteries.Dual m s (NotSmallerThan + err) String String
smallerThan min = Dual.dual' (Validators.smallerThan min)

inRange
  ∷ ∀ err m s. Applicative m
  ⇒ Applicative s
  ⇒ Range String
  → Batteries.Dual m s (NotInRange + err) String String
inRange range = Dual.dual' (Validators.inRange range)

equalTo
  ∷ ∀ err m s. Applicative m
  ⇒ Applicative s
  ⇒ String
  → Batteries.Dual m s (NotEqualTo + err) String String
equalTo min = Dual.dual' (Validators.equalTo min)

differentThan
  ∷ ∀ err m s. Applicative m
  ⇒ Applicative s
  ⇒ String
  → Batteries.Dual m s (NotDifferentThan + err) String String
differentThan n = Dual.dual' (Validators.differentThan n)

oneOf
  ∷ ∀ err m s. Applicative m
  ⇒ Applicative s
  ⇒ Array String
  → Batteries.Dual m s (NotOneOf + err) String String
oneOf arr = Dual.dual' (Validators.oneOf arr)

missingFrom
  ∷ ∀ err m s. Applicative m
  ⇒ Applicative s
  ⇒ Array String
  → Batteries.Dual m s (NotMissingFrom + err) String String
missingFrom arr = Dual.dual' (Validators.missingFrom arr)

