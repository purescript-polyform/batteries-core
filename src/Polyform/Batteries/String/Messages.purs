module Polyform.Batteries.String.Messages where

import Prelude
import Polyform.Batteries.Eq.Validators (NotEqualToErr)
import Polyform.Batteries.Generic.Ord.Validators (NotGreaterThanErr, NotSmallerThanErr, NotInRangeErr)
import Polyform.Batteries.Messages.Generic (notEqualTo, notGreaterThan, notInRange, notSmallerThan) as Generic
import Polyform.Batteries.String.Validators (NotEqualTo, NotGreaterThan, NotInRange, NotSmallerThan, NotEmptyExpected) as String
import Prim.Row (class Nub, class Union) as Row
import Record.Builder (Builder, merge) as Record.Builder
import Type.Row (type (+))

type Messages msgs
  = ( String.NotGreaterThan + String.NotSmallerThan
        + String.NotInRange
        + String.NotEqualTo
        + String.NotEmptyExpected
        + msgs
    )

type Printers
  = ( stringNotGreaterThan ∷ NotGreaterThanErr String → String
    , stringNotSmallerThan ∷ NotSmallerThanErr String → String
    , stringNotInRange ∷ NotInRangeErr String → String
    , stringNotEqualTo ∷ NotEqualToErr String → String
    , stringNotEmptyExpected ∷ Unit → String
    )

string ∷
  ∀ r r' r''.
  Row.Union r Printers r' ⇒
  Row.Nub r' r'' ⇒
  Record.Builder.Builder { | r } { | r'' }
string =
  let
    printers ∷ { | Printers }
    printers =
      { stringNotGreaterThan: Generic.notGreaterThan
      , stringNotSmallerThan: Generic.notSmallerThan
      , stringNotInRange: Generic.notInRange
      , stringNotEqualTo: Generic.notEqualTo
      , stringNotEmptyExpected: const $ "Non empty string expected"
      }
  in
    Record.Builder.merge printers
