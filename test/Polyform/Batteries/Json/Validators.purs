module Test.Polyform.Batteries.Json.Validators where

import Prelude

import Data.Argonaut (Json, fromNumber, fromObject, fromString)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (unV)
import Data.Variant (inj)
import Effect.Aff (Aff)
import Foreign.Object (fromFoldable) as Object
import Global.Unsafe (unsafeStringify)
import Polyform.Batteries.Json.Validators (Segment(..), _fieldMissing, _intExpected, consErrorsPath, field, liftErrors, int, number, object, string)
import Polyform.Batteries.Json.Validators (Validator) as Json
import Polyform.Validator (runValidator)
import Record.Extra (sequenceRecord)
import Test.Unit (TestSuite, failure, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (equal)


suite ∷ TestSuite
suite =
  Test.Unit.suite "Test.Polyform.Json.Validators" $ do
    test "Parse object" $ do
      let
        obj = object >>> d
          where
            d = sequenceRecord
              { foo: field "foo" int
              , bar: field "bar" string
              , baz: field "baz" number
              }
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
    test "Errors paths" $ do
      let
        obj ∷
          Json.Validator
          Aff
           ( fieldMissing :: Unit
           , intExpected :: Json
           , numberExpected :: Json
           , objectExpected :: Json
           , stringExpected :: Json
           )
          _

        obj = object >>> r
          where
            r = sequenceRecord
              { foo: field "foo" sub
              , bar: field "bar" string
              , baz: field "baz" number
              }
            sub = object >>> sequenceRecord
              { x: field "x" int
              , y: field "y" int
              }

        input = fromObject $ Object.fromFoldable
          [ Tuple "foo" $ fromObject $ Object.fromFoldable [ "x" /\ fromString "incorrect int" ]
          , "bar" /\ fromString "test"
          , "baz" /\ fromNumber 8.0
          ]
        expectedError = consErrorsPath (Key "foo") $
          consErrorsPath (Key "x") (liftErrors [ inj _intExpected (fromString "incorrect int") ])
          <> consErrorsPath (Key "y") (liftErrors [ inj _fieldMissing unit ])

      parsed ← runValidator obj input
      unV
        ( \err → when (err /= expectedError) $
            failure
              ( "Expecting \""
              <> unsafeStringify expectedError
              <> "\" but got: \""
              <> unsafeStringify err
              <> "\""
              )
        )
        (const $ failure ("Validation should fail"))
        parsed
