module Polyform.Batteries.Number.Validators where

import Prelude

import Polyform.Batteries (Validator) as Batteries
import Polyform.Batteries.Eq.Validators (NotEqualToErr)
import Polyform.Batteries.Eq.Validators (differentThan, equalTo, missingFrom, oneOf) as Generic.Eq.Validators
import Polyform.Batteries.Generic.Ord.Validators (NotGreaterThanErr, NotInRangeErr, NotSmallerThanErr, Range, greaterThan, inRange, smallerThan) as Generic.Ord.Validators
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

_notGreaterThan = SProxy ∷ SProxy "numberNotGreaterThan"

type NotGreaterThan e = (numberNotGreaterThan ∷ Generic.Ord.Validators.NotGreaterThanErr Number | e )

greaterThan
  ∷ ∀ m e
  . Applicative m
  ⇒ Number
  → Batteries.Validator m (NotGreaterThan + e) Number Number
greaterThan = Generic.Ord.Validators.greaterThan _notGreaterThan

_notSmallerThan = SProxy ∷ SProxy "numberNotSmallerThan"

type NotSmallerThan e = (numberNotSmallerThan ∷ Generic.Ord.Validators.NotSmallerThanErr Number | e )

smallerThan
  ∷ ∀ m e
  . Applicative m
  ⇒ Number
  → Batteries.Validator m (NotSmallerThan + e) Number Number
smallerThan = Generic.Ord.Validators.smallerThan _notSmallerThan

_notInRange = SProxy ∷ SProxy "numberNotInRange"

type NotInRange e = (numberNotInRange ∷ Generic.Ord.Validators.NotInRangeErr Number | e)

inRange
  ∷ ∀ m e
  . Applicative m
  ⇒ Generic.Ord.Validators.Range Number
  → Batteries.Validator m (NotInRange + e) Number Number
inRange = Generic.Ord.Validators.inRange _notInRange

_notEqualTo = SProxy ∷ SProxy "numberNotEqualTo"

type NotEqualTo e = (numberNotEqualTo ∷ NotEqualToErr Number | e)

equalTo
  ∷ ∀ m e
  . Applicative m
  ⇒ Number
  → Batteries.Validator m (NotEqualTo + e) Number Number
equalTo = Generic.Eq.Validators.equalTo _notEqualTo

type NotDifferentThan e = (numberNotDifferentThan ∷ Number | e)

_notDifferentThan = SProxy ∷ SProxy "numberNotDifferentThan"

differentThan
  ∷ ∀ m e
  . Applicative m
  ⇒ Number
  → Batteries.Validator m (NotDifferentThan + e) Number Number
differentThan = Generic.Eq.Validators.differentThan _notDifferentThan

type NotOneOf e = (numberNotOneOf ∷ Array Number | e)

_notOneOf = SProxy ∷ SProxy "numberNotOneOf"

oneOf
  ∷ ∀ m e
  . Applicative m
  ⇒ Array Number
  → Batteries.Validator m (NotOneOf + e) Number Number
oneOf = Generic.Eq.Validators.oneOf _notOneOf

type NotMissingFrom e = (numberNotMissingFrom ∷ Array Number | e)

_notMissingFrom = SProxy ∷ SProxy "numberNotMissingFrom"

missingFrom
  ∷ ∀ m e
  . Applicative m
  ⇒ Array Number
  → Batteries.Validator m (NotMissingFrom + e) Number Number
missingFrom = Generic.Eq.Validators.missingFrom _notMissingFrom

