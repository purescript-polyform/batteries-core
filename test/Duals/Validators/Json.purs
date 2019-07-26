module Test.Duals.Validators.Json where

import Prelude hiding (unit)

import Data.Argonaut (fromBoolean, fromNumber) as Argonaut
import Data.Argonaut (fromBoolean, fromNumber, fromObject, fromString, jsonNull, stringify, toObject)
import Data.Argonaut (fromString, stringify) as Argounaut
import Data.Argonaut.Core (Json)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (invalid, unV)
import Data.Variant (Variant, inj, match, onMatch)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Foreign.Object (Object, fromFoldable) as Object
import Global.Unsafe (unsafeStringify)
import Polyform.Dual (DualD(..), dual, parser, serializer, (~))
import Polyform.Dual.Record (build) as Dual.Record
import Polyform.Dual.Validators.UrlEncoded as Dual.Validators.UrlEncoded
import Polyform.Dual.Variant (case_)
import Polyform.Duals.Validators.Json (Dual, JsonDual, boolean, field, int, json, noArgs, number, object, on, string, sum, unit, (:=))
import Polyform.Validator (hoistFn, hoistFnMV, hoistFnV, runValidator, valid)
import Polyform.Validators.Json (JsonError, Validator)
import Polyform.Validators.Json (Validator, JsonDecodingError, failure, jsType) as Json
import Polyform.Validators.UrlEncoded as UrlEncoded
import Prelude (unit) as Prelude
import Record.Extra (sequenceRecord)
import Test.Unit (failure, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)
import Type.Prelude (SProxy(..), reflectSymbol)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)


data Sum = S String | I Int | B Boolean | N Number | U Unit | E
derive instance genericSum ∷ Generic Sum _
derive instance eqSum ∷ Eq Sum
instance showSum ∷ Show Sum where
  show = genericShow

data Single = Single String
derive instance genericSingle ∷ Generic Single _

variant = case_
  # on (SProxy ∷ SProxy "s") string
  # on (SProxy ∷ SProxy "u") unit
  # on (SProxy ∷ SProxy "i") int

-- sumVariantDual ∷ ∀ e. JsonDual Aff (Json.JsonDecodingError e) SumV
-- sumVariantDual = Dual.object >>> tagWithValue >>> valueDual
--   where
--     tagWithValue = Dual $ { t: _, v: _ }
--       <$> _.t ~ Dual.Json.objectField "tag" Dual.Json.string
--       <*> _.v ~ Dual.Json.objectField "value" identity
-- 
--     parser = hoistFnMV $ case _ of
--       { t: "s", v } → runValidator (Json.string >>> hoistFn (inj _s)) v
--       { t: "i", v } → runValidator (Json.int >>> hoistFn (inj _i)) v
--       { t: "b", v } → runValidator (Json.boolean >>> hoistFn (inj _b)) v
--       { t, v } → pure $ Json.failure
--         ("Invalid tag: " <> t <> " with value: " <> (stringify v) )
-- 
--     serializer = match
--       { s: \s → { t: "s", v: Argounaut.fromString s }
--       , i: \i → { t: "i", v: Argonaut.fromNumber <<< toNumber $ i }
--       , b: \b → { t: "b", v: Argonaut.fromBoolean $ b }
--       }
-- 
--     valueDual = dual { parser, serializer }


suite =
  Test.Unit.suite "Test.Duals.Validators.Json" $ do
    -- Test.Unit.suite "variant handling" $ do

    Test.Unit.suite "record handling" $ do
      let
        obj ∷ ∀ e m. Monad m ⇒ JsonDual m e { foo ∷ Int, bar ∷ String, baz ∷ Number }
        obj = object >>> d
          where
            d = Dual.Record.build
              $ (SProxy ∷ SProxy "foo") := int
              <<< (SProxy ∷ SProxy "bar") := string
              <<< (SProxy ∷ SProxy "baz") := number

      test "Parse object" $ do
        let
          input = fromObject $ Object.fromFoldable
            [ "foo" /\ fromNumber (toNumber 8)
            , "bar" /\ fromString "test"
            , "baz" /\ fromNumber 8.0
            ]
          expected = { foo: 8, bar: "test", baz: 8.0 }
        parsed ← runValidator (parser obj) input
        unV
          (const $ failure "Validation failed")
          (_ `equal` expected)
          parsed

    Test.Unit.suite "sum handling" $ do
      test "through generic helper" $ do
        let
          sumD ∷
            ∀ e m.
            Monad m ⇒
            JsonDual m e Sum
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
