module Polyform.Batteries.Messages.UrlEncoded where

import Prelude

import Data.Foldable (intercalate)
import Polyform.Batteries.Number (NumberExpected)
import Polyform.Batteries.UrlEncoded.Query (Value) as Query
import Polyform.Batteries.UrlEncoded.Validators (BooleanExpected, MissingValue)
import Prim.Row (class Nub, class Union) as Row
import Record.Builder (Builder, merge) as Record.Builder
import Type.Row (type (+))

type Messages msgs =
  ( BooleanExpected
  + MissingValue
  + NumberExpected
  + msgs
  )

type Printers =
  ( booleanExpected ∷ Query.Value → String
  , intExpected ∷ String → String
  , numberExpected ∷ String → String
  , missingValue ∷ Unit → String
  )

printers ∷ { | Printers }
printers =
  { booleanExpected:
      append "Expected a boolean value but got " <<< intercalate ","
  , intExpected: append "Expecting an int value but got " <<< show
  , numberExpected: append "Expecting a number value but got " <<< show
  , missingValue: const "This value is required"
  }

urlEncoded
  ∷ ∀ r r' r''
  . Row.Union r Printers r'
  ⇒ Row.Nub r' r''
  ⇒ Record.Builder.Builder { | r } { | r'' }
urlEncoded = Record.Builder.merge printers

