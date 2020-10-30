module Polyform.Batteries.Messages.Json where

-- | TODO: Provide a way to flatten represntation of json errors (with paths)
-- | Provide a helper which flattens any errors using "unsafeStringify" or something.
import Prelude
import Data.Argonaut (Json, fromObject)
import Foreign.Object (fromHomogeneous)
import Polyform.Batteries.Json (ArrayExpected, BooleanExpected, FieldMissing, IntExpected, NullExpected, NumberExpected, ObjectExpected, StringExpected) as Json
import Prim.Row (class Nub, class Union) as Row
import Prim.RowList (class RowToList)
import Record.Builder (Builder, merge) as Record.Builder
import Type.Row (type (+))
import Type.Row.Homogeneous (class HomogeneousRowList)

type FieldMessages msgs
  = ( Json.ArrayExpected
        + Json.BooleanExpected
        + Json.FieldMissing
        + Json.IntExpected
        + Json.NullExpected
        + Json.NumberExpected
        + Json.ObjectExpected
        + Json.StringExpected
        + msgs
    )

type FieldPrinters
  = ( "arrayExpected" ∷ Json → Json
    , "booleanExpected" ∷ Json → Json
    , "fieldMissing" ∷ Json → Json
    , "intExpected" ∷ Json → Json
    , "nullExpected" ∷ Json → Json
    , "numberExpected" ∷ Json → Json
    , "objectExpected" ∷ Json → Json
    , "stringExpected" ∷ Json → Json
    )

field ∷
  ∀ r r' r''.
  Row.Union r FieldPrinters r' ⇒
  Row.Nub r' r'' ⇒
  Record.Builder.Builder { | r } { | r'' }
field =
  let
    flatten ∷ ∀ s sl. RowToList s sl ⇒ HomogeneousRowList sl Json ⇒ Record s → Json
    flatten = fromObject <<< fromHomogeneous

    printers ∷ { | FieldPrinters }
    printers =
      { arrayExpected: \i → flatten { arrayExpected: i }
      , booleanExpected: \i → flatten { booleanExpected: i }
      , fieldMissing: \i → flatten { fieldMissing: i }
      , intExpected: \i → flatten { intExpected: i }
      , nullExpected: \i → flatten { nullExpected: i }
      , numberExpected: \i → flatten { numberExpected: i }
      , objectExpected: \i → flatten { objectExpected: i }
      , stringExpected: \i → flatten { stringExpected: i }
      }
  in
    Record.Builder.merge printers
