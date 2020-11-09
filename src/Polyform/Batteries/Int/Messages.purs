module Polyform.Batteries.Int.Messages where

import Prelude
import Polyform.Batteries.Int (IntExpected)
import Prim.Row (class Nub, class Union) as Row
import Record.Builder (Builder, merge) as Record.Builder
import Type.Row (type (+))

type Messages msgs
  = ( IntExpected
        + msgs
    )

type Printers
  = ( intExpected ∷ String → String
    )

printers ∷ { | Printers }
printers = { intExpected: append "Expecting an int value but got " <<< show }

int ∷
  ∀ r r' r''.
  Row.Union r Printers r' ⇒
  Row.Nub r' r'' ⇒
  Record.Builder.Builder { | r } { | r'' }
int = Record.Builder.merge printers
