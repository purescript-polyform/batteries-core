module Test.Polyform.Json.Duals where

import Prelude hiding (unit)

import Data.Argonaut (Json, fromNumber, fromObject, fromString, jsonNull)
import Data.Argonaut (fromBoolean, fromNumber) as Argonaut
import Data.Argonaut (fromString) as Argounaut
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (invalid, unV)
import Data.Variant (Variant, inj, match)
import Effect.Aff (Aff)
import Foreign.Object (fromFoldable) as Object
import Global.Unsafe (unsafeStringify)
import Polyform.Dual (Dual(..)) as Dual
import Polyform.Dual (dual, parser, (~))
import Polyform.Dual.Record (build) as Dual.Record
import Polyform.Dual.Variant (case_)
import Polyform.Json.Duals (CoproductErrors, IncorrectTag, _incorrectTag, arrayOf, boolean, int, noArgs, number, object, on, string, sum, unit, (:=))
import Polyform.Json.Duals (Dual, field, string) as Json.Duals
import Polyform.Json.Duals (object) as Json.Dual
import Polyform.Json.Validators (BooleanExpected, FieldMissing, IntExpected, NumberExpected, ObjectExpected, StringExpected, _stringExpected)
import Polyform.Json.Validators (boolean, error, int, string) as Json.Validators
import Polyform.Validator (liftFn) as Validator
import Polyform.Validator (liftFnMV) as Validtor
import Polyform.Validator (runValidator)
import Prelude (unit) as Prelude
import Test.Unit (TestSuite, failure, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (equal)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

data Sum = S String | I Int | B Boolean | N Number | U Unit | E
derive instance genericSum ∷ Generic Sum _
derive instance eqSum ∷ Eq Sum
instance showSum ∷ Show Sum where
  show = genericShow

data Single = Single String
derive instance genericSingle ∷ Generic Single _

_b = SProxy ∷ SProxy "b"
_s = SProxy ∷ SProxy "s"
_u = SProxy ∷ SProxy "u"
_i = SProxy ∷ SProxy "i"

variant
  ∷ forall e m s. Monad s
  ⇒ Monad m
  ⇒ Json.Duals.Dual m s (CoproductErrors + IntExpected + e) Json (Variant (i ∷ Int, s ∷ String, u ∷ Unit))
variant = case_
  # on (SProxy ∷ SProxy "s") string
  # on (SProxy ∷ SProxy "u") unit
  # on (SProxy ∷ SProxy "i") int

sumVariantDual
  ∷ ∀ e s. Monad s
  ⇒ Json.Duals.Dual Aff s
    ( BooleanExpected
    + FieldMissing
    + IncorrectTag
    + IntExpected
    + ObjectExpected
    + StringExpected
    + e
    )
    Json
    (Variant (s ∷ String, b ∷ Boolean, i ∷ Int))
sumVariantDual = Json.Dual.object >>> tagWithValue >>> valueDual
  where
    tagWithValue = Dual.Dual $ { t: _, v: _ }
      <$> _.t ~ Json.Duals.field "tag" Json.Duals.string
      <*> _.v ~ Json.Duals.field "value" identity

    parser = Validtor.liftFnMV $ case _ of
      { t: "s", v } → runValidator (Json.Validators.string >>> Validator.liftFn (inj _s)) v
      { t: "i", v } → runValidator (Json.Validators.int >>> Validator.liftFn (inj _i)) v
      { t: "b", v } → runValidator (Json.Validators.boolean >>> Validator.liftFn (inj _b)) v
      { t, v } → pure $ invalid $ Json.Validators.error _incorrectTag t

    serializer = match
      { s: \s → pure { t: "s", v: Argounaut.fromString s }
      , i: \i → pure { t: "i", v: Argonaut.fromNumber <<< toNumber $ i }
      , b: \b → pure { t: "b", v: Argonaut.fromBoolean $ b }
      }

    valueDual = dual parser serializer

suite :: TestSuite
suite =
  Test.Unit.suite "Test.Json.Validators.Duals" $ do

    Test.Unit.suite "record handling" $ do
      let
        obj
          ∷ ∀ e
          . Json.Duals.Dual Aff Identity
            ( FieldMissing
            + IntExpected
            + NumberExpected
            + ObjectExpected
            + StringExpected
            + e
            )
            Json
            { foo ∷ Int, bar ∷ String, baz ∷ Number }
        obj = object >>> d
          where
            d = Dual.Record.build
              $ (SProxy ∷ SProxy "foo") := int
              <<< (SProxy ∷ SProxy "bar") := string
              <<< (SProxy ∷ SProxy "baz") := number
        objs = arrayOf obj

      test "Parse object" $ do
        let
          input = fromObject $ Object.fromFoldable
            [ "foo" /\ fromNumber (toNumber 8)
            , "bar" /\ fromString "test"
            , "baz" /\ fromNumber 8.0
            ]
          expected = { foo: 8, bar: "test", baz: 8.0 }
        parsed ← runValidator (parser obj) input
        -- void $ runSerializer objs []
        unV
          (const $ failure "Validation failed")
          (_ `equal` expected)
          parsed

    Test.Unit.suite "sum handling" $ do
      test "through generic helper" $ do
        let
          sumD
            ∷ Json.Duals.Dual
              Aff
              Identity
              ( BooleanExpected
              + FieldMissing
              + IntExpected
              + IncorrectTag
              + NumberExpected
              + ObjectExpected
              + StringExpected
              + ()
              )
              Json
              Sum
          sumD = sum
            { "S": identity string
            , "I": identity int
            , "B": identity boolean
            , "N": identity number
            , "E": identity noArgs
            , "U": identity unit
            }

          s = fromObject $ Object.fromFoldable
            [ "tag" /\ fromString "S", "value" /\ fromString "test" ]
          e = fromObject $ Object.fromFoldable
            [ "tag" /\ fromString "E", "value" /\ jsonNull ]
          n = fromObject $ Object.fromFoldable
            [ "tag" /\ fromString "N", "value" /\ fromNumber 8.0 ]
          u = fromObject $ Object.fromFoldable
            [ "tag" /\ fromString "U", "value" /\ jsonNull ]

        parsedS ← runValidator (parser sumD) s
        unV
          (const $ failure "Validation failed")
          (_ `equal` (S "test"))
          parsedS

        parsedE ← runValidator (parser sumD) e
        unV
          (const $ failure "Validation failed")
          (_ `equal` E)
          parsedE


        parsedN ← runValidator (parser sumD) n
        unV
          (const $ failure "Validation failed")
          (_ `equal` (N 8.0))
          parsedN

        parsedU ← runValidator (parser sumD) u
        unV
          (const $ failure "Validation failed")
          (_ `equal` (U Prelude.unit))
          parsedU

        let
          s' = fromObject $ Object.fromFoldable
            [ "tag" /\ fromString "S", "value" /\ fromNumber 8.0 ]
          _json = SProxy ∷ SProxy "json"
          expectedError
            = Json.Validators.error _stringExpected (Argonaut.fromNumber 8.0)
            <> Json.Validators.error _incorrectTag "S"
            <> Json.Validators.error _incorrectTag "S"
            <> Json.Validators.error _incorrectTag "S"
            <> Json.Validators.error _incorrectTag "S"
            <> Json.Validators.error _incorrectTag "S"
        parsedS' ← runValidator (parser sumD) s'

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
          (const $ failure "Expecting validation failure")
          parsedS'
