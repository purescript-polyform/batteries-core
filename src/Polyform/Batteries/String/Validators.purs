module Polyform.Batteries.String.Validators where

import Prelude

import Polyform.Batteries (Validator) as Batteries
import Polyform.Batteries.Generic.Eq.Validators (NotEqualToErr, NotOneOfErr, NotMissingFromErr)
import Polyform.Batteries.Generic.Eq.Validators (differentThan, equalTo, missingFrom, oneOf) as Generic.Eq.Validators
import Polyform.Batteries.Generic.Messages (emptyExpected, notDifferentThan, notEmptyExpected, notEqualTo, notGreaterThan, notInRange, notMissingFrom, notOneOf, notSmallerThan) as Generic.Messages
import Polyform.Batteries.Generic.Monoid.Validators (isEmpty, isNotEmpty) as Generic.Monoid.Validators
import Polyform.Batteries.Generic.Ord.Validators (NotGreaterThanErr, NotInRangeErr, NotSmallerThanErr, Range, greaterThan, inRange, smallerThan) as Generic.Ord.Validators
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

_notGreaterThan = SProxy ∷ SProxy "stringNotGreaterThan"

type NotGreaterThan e
  = ( stringNotGreaterThan ∷ Generic.Ord.Validators.NotGreaterThanErr String | e )

greaterThan ∷
  ∀ m e.
  Applicative m ⇒
  String →
  Batteries.Validator m (NotGreaterThan + e) String String
greaterThan = Generic.Ord.Validators.greaterThan _notGreaterThan Generic.Messages.notGreaterThan

_notSmallerThan = SProxy ∷ SProxy "stringNotSmallerThan"

type NotSmallerThan e
  = ( stringNotSmallerThan ∷ Generic.Ord.Validators.NotSmallerThanErr String | e )

smallerThan ∷
  ∀ m e.
  Applicative m ⇒
  String →
  Batteries.Validator m (NotSmallerThan + e) String String
smallerThan = Generic.Ord.Validators.smallerThan _notSmallerThan Generic.Messages.notSmallerThan

_notInRange = SProxy ∷ SProxy "stringNotInRange"

type NotInRange e
  = ( stringNotInRange ∷ Generic.Ord.Validators.NotInRangeErr String | e )

inRange ∷
  ∀ m e.
  Applicative m ⇒
  Generic.Ord.Validators.Range String →
  Batteries.Validator m (NotInRange + e) String String
inRange = Generic.Ord.Validators.inRange _notInRange Generic.Messages.notInRange

_notEqualTo = SProxy ∷ SProxy "stringNotEqualTo"

type NotEqualTo e
  = ( stringNotEqualTo ∷ NotEqualToErr String | e )

equalTo ∷
  ∀ m e.
  Applicative m ⇒
  String →
  Batteries.Validator m (NotEqualTo + e) String String
equalTo = Generic.Eq.Validators.equalTo _notEqualTo Generic.Messages.notEqualTo

type NotDifferentThan e
  = ( stringNotDifferentThan ∷ String | e )

_notDifferentThan = SProxy ∷ SProxy "stringNotDifferentThan"

differentThan ∷
  ∀ m e.
  Applicative m ⇒
  String →
  Batteries.Validator m (NotDifferentThan + e) String String
differentThan = Generic.Eq.Validators.differentThan _notDifferentThan Generic.Messages.notDifferentThan

type NotOneOf e
  = ( stringNotOneOf ∷ NotOneOfErr String | e )

_notOneOf = SProxy ∷ SProxy "stringNotOneOf"

oneOf ∷
  ∀ m e.
  Applicative m ⇒
  Array String →
  Batteries.Validator m (NotOneOf + e) String String
oneOf = Generic.Eq.Validators.oneOf _notOneOf Generic.Messages.notOneOf

type NotMissingFrom e
  = ( stringNotMissingFrom ∷ NotMissingFromErr String | e )

_notMissingFrom = SProxy ∷ SProxy "stringNotMissingFrom"

missingFrom ∷
  ∀ m e.
  Applicative m ⇒
  Array String →
  Batteries.Validator m (NotMissingFrom + e) String String
missingFrom = Generic.Eq.Validators.missingFrom _notMissingFrom Generic.Messages.notMissingFrom

type NotEmptyExpected e
  = ( stringNotEmptyExpected ∷ Unit | e )

_nonEmptyExpected = SProxy ∷ SProxy "stringNotEmptyExpected"

isNotEmpty ∷
  ∀ m e.
  Applicative m ⇒
  Batteries.Validator m (NotEmptyExpected + e) String String
isNotEmpty = Generic.Monoid.Validators.isNotEmpty _nonEmptyExpected Generic.Messages.notEmptyExpected

type EmptyExpected e
  = ( stringEmptyExpected ∷ String | e )

_emptyExpected = SProxy ∷ SProxy "stringEmptyExpected"

isEmpty ∷
  ∀ m e.
  Applicative m ⇒
  Batteries.Validator m (EmptyExpected + e) String String
isEmpty = Generic.Monoid.Validators.isEmpty _emptyExpected Generic.Messages.emptyExpected
