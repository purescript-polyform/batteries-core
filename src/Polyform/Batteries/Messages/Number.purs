module Polyform.Batteries.Messages.Number where

import Polyform.Batteries.Eq.Validators (NotEqualToErr)
import Polyform.Batteries.Generic.Ord.Validators (NotGreaterThanErr, NotInRangeErr, NotSmallerThanErr)
import Polyform.Batteries.Messages.Generic (notEqualTo, notGreaterThan, notInRange, notSmallerThan) as Generic
import Polyform.Batteries.Number.Validators (NotEqualTo, NotGreaterThan, NotSmallerThan, NotInRange) as Number
import Prim.Row (class Nub, class Union) as Row
import Record.Builder (Builder, merge) as Record.Builder
import Type.Row (type (+))

type Messages msgs =
  ( Number.NotGreaterThan + Number.NotSmallerThan
  + Number.NotInRange + Number.NotEqualTo
  + msgs
  )

type Printers =
  ( numberNotGreaterThan ∷ NotGreaterThanErr Number → String
  , numberNotSmallerThan ∷ NotSmallerThanErr Number → String
  , numberNotInRange ∷ NotInRangeErr Number → String
  , numberNotEqualTo ∷ NotEqualToErr Number → String
  )

number
  ∷ ∀ r r' r''
  . Row.Union r Printers r'
  ⇒ Row.Nub r' r''
  ⇒ Record.Builder.Builder { | r } { | r'' }
number =
  let
    printers ∷ { | Printers }
    printers =
      { numberNotGreaterThan: Generic.notGreaterThan
      , numberNotSmallerThan: Generic.notSmallerThan
      , numberNotInRange: Generic.notInRange
      , numberNotEqualTo: Generic.notEqualTo
      }
  in
    Record.Builder.merge printers

