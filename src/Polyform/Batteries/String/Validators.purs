module Polyform.Batteries.String.Validators where

import Prelude

import Polyform.Batteries (Validator) as Batteries
import Polyform.Batteries.Eq.Validators (NotEqualToErr)
import Polyform.Batteries.Eq.Validators (differentThan, equalTo, missingFrom, oneOf) as Generic.Eq.Validators
import Polyform.Batteries.Generic.Ord.Validators (NotGreaterThanErr, NotInRangeErr, NotSmallerThanErr, Range, greaterThan, inRange, smallerThan) as Generic.Ord.Validators
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

_notGreaterThan = SProxy ∷ SProxy "stringNotGreaterThan"

type NotGreaterThan e = (stringNotGreaterThan ∷ Generic.Ord.Validators.NotGreaterThanErr String | e )

greaterThan
  ∷ ∀ m e
  . Applicative m
  ⇒ String
  → Batteries.Validator m (NotGreaterThan + e) String String
greaterThan = Generic.Ord.Validators.greaterThan _notGreaterThan

_notSmallerThan = SProxy ∷ SProxy "stringNotSmallerThan"

type NotSmallerThan e = (stringNotSmallerThan ∷ Generic.Ord.Validators.NotSmallerThanErr String | e )

smallerThan
  ∷ ∀ m e
  . Applicative m
  ⇒ String
  → Batteries.Validator m (NotSmallerThan + e) String String
smallerThan = Generic.Ord.Validators.smallerThan _notSmallerThan

_notInRange = SProxy ∷ SProxy "stringNotInRange"

type NotInRange e = (stringNotInRange ∷ Generic.Ord.Validators.NotInRangeErr String | e)

inRange
  ∷ ∀ m e
  . Applicative m
  ⇒ Generic.Ord.Validators.Range String
  → Batteries.Validator m (NotInRange + e) String String
inRange = Generic.Ord.Validators.inRange _notInRange

_notEqualTo = SProxy ∷ SProxy "stringNotEqualTo"

type NotEqualTo e = (stringNotEqualTo ∷ NotEqualToErr String | e)

equalTo
  ∷ ∀ m e
  . Applicative m
  ⇒ String
  → Batteries.Validator m (NotEqualTo + e) String String
equalTo = Generic.Eq.Validators.equalTo _notEqualTo

type NotDifferentThan e = (stringNotDifferentThan ∷ String | e)

_notDifferentThan = SProxy ∷ SProxy "stringNotDifferentThan"

differentThan
  ∷ ∀ m e
  . Applicative m
  ⇒ String
  → Batteries.Validator m (NotDifferentThan + e) String String
differentThan = Generic.Eq.Validators.differentThan _notDifferentThan

type NotOneOf e = (stringNotOneOf ∷ Array String | e)

_notOneOf = SProxy ∷ SProxy "stringNotOneOf"

oneOf
  ∷ ∀ m e
  . Applicative m
  ⇒ Array String
  → Batteries.Validator m (NotOneOf + e) String String
oneOf = Generic.Eq.Validators.oneOf _notOneOf

type NotMissingFrom e = (stringNotMissingFrom ∷ Array String | e)

_notMissingFrom = SProxy ∷ SProxy "stringNotMissingFrom"

missingFrom
  ∷ ∀ m e
  . Applicative m
  ⇒ Array String
  → Batteries.Validator m (NotMissingFrom + e) String String
missingFrom = Generic.Eq.Validators.missingFrom _notMissingFrom

