module Polyform.Batteries.Messages.UrlEncoded where

import Prelude

import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Polyform.Batteries.Number (NumberExpected)
import Polyform.Batteries.UrlEncoded.Query (Value) as Query
import Polyform.Batteries.UrlEncoded.Validators (BooleanExpected, SingleValueExpected, IntExpected)
import Prim.Row (class Nub, class Union) as Row
import Record.Builder (Builder, merge) as Record.Builder
import Type.Row (type (+))

type Messages msgs =
  ( BooleanExpected
  + SingleValueExpected
  + IntExpected
  + NumberExpected
  + msgs
  )

type Printers =
  ( booleanExpected ∷ Query.Value → String
  , intExpected ∷ String → String
  , numberExpected ∷ String → String
  , singleValueExpected ∷ Maybe Query.Value → String
  )

printers ∷ { | Printers }
printers =
  { booleanExpected:
      append "Expected a boolean value but got " <<< intercalate ","
  , intExpected: append "Expecting an int value but got " <<< show
  , numberExpected: append "Expecting a number value but got " <<< show
  , singleValueExpected: case _ of
    Just [] → "Value missing"
    Nothing → "Value missing"
    Just arr → "Expecting a single value but multiple values: " <> intercalate "," arr
  }

urlEncoded
  ∷ ∀ r r' r''
  . Row.Union r Printers r'
  ⇒ Row.Nub r' r''
  ⇒ Record.Builder.Builder { | r } { | r'' }
urlEncoded = Record.Builder.merge printers

