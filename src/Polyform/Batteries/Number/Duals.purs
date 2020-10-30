module Polyform.Batteries.Number.Duals where

import Prelude
import Polyform.Batteries (Dual) as Batteries
import Polyform.Batteries.Generic.Ord.Validators (Range)
import Polyform.Batteries.Number.Validators (NotEqualTo, NotGreaterThan, NotInRange, NotMissingFrom, NotOneOf, NotSmallerThan, NotDifferentThan)
import Polyform.Batteries.Number.Validators (differentThan, equalTo, greaterThan, inRange, missingFrom, oneOf, smallerThan) as Validators
import Polyform.Dual (dual') as Dual
import Type.Row (type (+))

greaterThan ∷
  ∀ err m.
  Applicative m ⇒
  Number →
  Batteries.Dual m (NotGreaterThan + err) Number Number
greaterThan max = Dual.dual' (Validators.greaterThan max)

smallerThan ∷
  ∀ err m.
  Applicative m ⇒
  Number →
  Batteries.Dual m (NotSmallerThan + err) Number Number
smallerThan min = Dual.dual' (Validators.smallerThan min)

inRange ∷
  ∀ err m.
  Applicative m ⇒
  Range Number →
  Batteries.Dual m (NotInRange + err) Number Number
inRange range = Dual.dual' (Validators.inRange range)

equalTo ∷
  ∀ err m.
  Applicative m ⇒
  Number →
  Batteries.Dual m (NotEqualTo + err) Number Number
equalTo min = Dual.dual' (Validators.equalTo min)

differentThan ∷
  ∀ err m.
  Applicative m ⇒
  Number →
  Batteries.Dual m (NotDifferentThan + err) Number Number
differentThan n = Dual.dual' (Validators.differentThan n)

oneOf ∷
  ∀ err m.
  Applicative m ⇒
  Array Number →
  Batteries.Dual m (NotOneOf + err) Number Number
oneOf arr = Dual.dual' (Validators.oneOf arr)

missingFrom ∷
  ∀ err m.
  Applicative m ⇒
  Array Number →
  Batteries.Dual m (NotMissingFrom + err) Number Number
missingFrom arr = Dual.dual' (Validators.missingFrom arr)
