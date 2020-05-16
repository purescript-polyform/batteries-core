module Test.Polyform.Json.Validators where

import Prelude

import Data.Argonaut (fromNumber, fromObject, fromString)
import Data.Int (toNumber)
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (unV)
import Foreign.Object (fromFoldable) as Object
import Polyform.Json.Validators (Validator, field, int, number, object, string)
import Polyform.Validator (runValidator)
import Record.Extra (sequenceRecord)
import Test.Unit (TestSuite, failure, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (equal)

obj ∷ ∀ e m. Monad m ⇒ Validator m e { foo ∷ Int, bar ∷ String, baz ∷ Number }
obj = object >>> d
  where
    d = sequenceRecord
      { foo: field "foo" int
      , bar: field "bar" string
      , baz: field "baz" number
      }

suite ∷ TestSuite
suite =
  Test.Unit.suite "Test.Polyform.Json.Validators" $ do
    test "Parse object" $ do
      let
        input = fromObject $ Object.fromFoldable
          [ "foo" /\ fromNumber (toNumber 8)
          , "bar" /\ fromString "test"
          , "baz" /\ fromNumber 8.0
          ]
        expected = { foo: 8, bar: "test", baz: 8.0 }
      parsed ← runValidator obj input
      unV
        (const $ failure "Validation failed")
        (_ `equal` expected)
        parsed
