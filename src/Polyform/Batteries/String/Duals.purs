module Polyform.Batteries.String.Duals where

import Prelude

import Polyform.Batteries (Dual') as Batteries
import Polyform.Batteries.Generic.Ord.Validators (Range)
import Polyform.Batteries.String.Validators (EmptyExpected, NotDifferentThan, NotEqualTo, NotGreaterThan, NotInRange, NotMissingFrom, NotOneOf, NotSmallerThan, NotEmptyExpected)
import Polyform.Batteries.String.Validators (differentThan, equalTo, greaterThan, inRange, isEmpty, isNotEmpty, missingFrom, oneOf, smallerThan) as Validators
import Polyform.Dual (dual') as Dual
import Type.Row (type (+))

greaterThan ∷
  ∀ err m.
  Applicative m ⇒
  String →
  Batteries.Dual' m (NotGreaterThan + err) String String
greaterThan max = Dual.dual' (Validators.greaterThan max)

smallerThan ∷
  ∀ err m.
  Applicative m ⇒
  String →
  Batteries.Dual' m (NotSmallerThan + err) String String
smallerThan min = Dual.dual' (Validators.smallerThan min)

inRange ∷
  ∀ err m.
  Applicative m ⇒
  Range String →
  Batteries.Dual' m (NotInRange + err) String String
inRange range = Dual.dual' (Validators.inRange range)

equalTo ∷
  ∀ err m.
  Applicative m ⇒
  String →
  Batteries.Dual' m (NotEqualTo + err) String String
equalTo min = Dual.dual' (Validators.equalTo min)

differentThan ∷
  ∀ err m.
  Applicative m ⇒
  String →
  Batteries.Dual' m (NotDifferentThan + err) String String
differentThan n = Dual.dual' (Validators.differentThan n)

oneOf ∷
  ∀ err m.
  Applicative m ⇒
  Array String →
  Batteries.Dual' m (NotOneOf + err) String String
oneOf arr = Dual.dual' (Validators.oneOf arr)

missingFrom ∷
  ∀ err m.
  Applicative m ⇒
  Array String →
  Batteries.Dual' m (NotMissingFrom + err) String String
missingFrom arr = Dual.dual' (Validators.missingFrom arr)

isNotEmpty ∷
  ∀ err m.
  Applicative m ⇒
  Batteries.Dual' m (NotEmptyExpected + err) String String
isNotEmpty = Dual.dual' Validators.isNotEmpty

isEmpty ∷
  ∀ err m.
  Applicative m ⇒
  Batteries.Dual' m (EmptyExpected + err) String String
isEmpty = Dual.dual' Validators.isEmpty
