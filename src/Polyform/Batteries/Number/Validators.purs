module Polyform.Batteries.Number.Validators where

import Prelude

import Polyform.Batteries (Validator') as Batteries
import Polyform.Batteries.Generic.Eq.Validators (NotEqualToErr, NotOneOfErr, NotMissingFromErr)
import Polyform.Batteries.Generic.Eq.Validators (differentThan, equalTo, missingFrom, oneOf) as Generic.Eq.Validators
import Polyform.Batteries.Generic.Messages (notDifferentThan, notEqualTo, notGreaterThan, notInRange, notMissingFrom, notOneOf, notSmallerThan) as Generic.Messages
import Polyform.Batteries.Generic.Ord.Validators (NotGreaterThanErr, NotInRangeErr, NotSmallerThanErr, Range, greaterThan, inRange, smallerThan) as Generic.Ord.Validators
import Type.Proxy (Proxy(..))
import Type.Row (type (+))

_notGreaterThan = Proxy ∷ Proxy "numberNotGreaterThan"

type NotGreaterThan e
  = ( numberNotGreaterThan ∷ Generic.Ord.Validators.NotGreaterThanErr Number | e )

greaterThan ∷
  ∀ m e.
  Applicative m ⇒
  Number →
  Batteries.Validator' m (NotGreaterThan + e) Number Number
greaterThan = Generic.Ord.Validators.greaterThan _notGreaterThan Generic.Messages.notGreaterThan

_notSmallerThan = Proxy ∷ Proxy "numberNotSmallerThan"

type NotSmallerThan e
  = ( numberNotSmallerThan ∷ Generic.Ord.Validators.NotSmallerThanErr Number | e )

smallerThan ∷
  ∀ m e.
  Applicative m ⇒
  Number →
  Batteries.Validator' m (NotSmallerThan + e) Number Number
smallerThan = Generic.Ord.Validators.smallerThan _notSmallerThan Generic.Messages.notSmallerThan

_notInRange = Proxy ∷ Proxy "numberNotInRange"

type NotInRange e
  = ( numberNotInRange ∷ Generic.Ord.Validators.NotInRangeErr Number | e )

inRange ∷
  ∀ m e.
  Applicative m ⇒
  Generic.Ord.Validators.Range Number →
  Batteries.Validator' m (NotInRange + e) Number Number
inRange = Generic.Ord.Validators.inRange _notInRange Generic.Messages.notInRange

_notEqualTo = Proxy ∷ Proxy "numberNotEqualTo"

type NotEqualTo e
  = ( numberNotEqualTo ∷ NotEqualToErr Number | e )

equalTo ∷
  ∀ m e.
  Applicative m ⇒
  Number →
  Batteries.Validator' m (NotEqualTo + e) Number Number
equalTo = Generic.Eq.Validators.equalTo _notEqualTo Generic.Messages.notEqualTo

type NotDifferentThan e
  = ( numberNotDifferentThan ∷ Number | e )

_notDifferentThan = Proxy ∷ Proxy "numberNotDifferentThan"

differentThan ∷
  ∀ m e.
  Applicative m ⇒
  Number →
  Batteries.Validator' m (NotDifferentThan + e) Number Number
differentThan = Generic.Eq.Validators.differentThan _notDifferentThan Generic.Messages.notDifferentThan

type NotOneOf e
  = ( numberNotOneOf ∷ NotOneOfErr Number | e )

_notOneOf = Proxy ∷ Proxy "numberNotOneOf"

oneOf ∷
  ∀ m e.
  Applicative m ⇒
  Array Number →
  Batteries.Validator' m (NotOneOf + e) Number Number
oneOf = Generic.Eq.Validators.oneOf _notOneOf Generic.Messages.notOneOf

type NotMissingFrom e
  = ( numberNotMissingFrom ∷ NotMissingFromErr Number | e )

_notMissingFrom = Proxy ∷ Proxy "numberNotMissingFrom"

missingFrom ∷
  ∀ m e.
  Applicative m ⇒
  Array Number →
  Batteries.Validator' m (NotMissingFrom + e) Number Number
missingFrom = Generic.Eq.Validators.missingFrom _notMissingFrom Generic.Messages.notMissingFrom
