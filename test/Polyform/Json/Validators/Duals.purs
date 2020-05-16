module Test.Polyform.Json.Validators.Duals where

import Prelude hiding (unit)

import Data.Argonaut (fromNumber, fromObject, fromString, jsonNull)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.List (List(..)) as List
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (unV)
import Data.Variant (Variant, inj)
import Effect.Aff (Aff)
import Foreign.Object (fromFoldable) as Object
import Polyform.Dual (parser)
import Polyform.Dual.Record (build) as Dual.Record
import Polyform.Dual.Variant (case_)
import Polyform.Json.Validators.Duals (JsonDual, arrayOf, boolean, int, noArgs, number, object, on, string, sum, unit, (:=))
import Polyform.Json.Validators.Duals (JsonDual) as Json.Validators.Dual
import Polyform.Validator (runValidator)
import Prelude (unit) as Prelude
import Test.Unit (TestSuite, failure, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (equal)
import Type.Prelude (SProxy(..))

data Sum = S String | I Int | B Boolean | N Number | U Unit | E
derive instance genericSum ∷ Generic Sum _
derive instance eqSum ∷ Eq Sum
instance showSum ∷ Show Sum where
  show = genericShow

data Single = Single String
derive instance genericSingle ∷ Generic Single _

variant ∷ forall e m s. Monad s ⇒ Monad m ⇒ Json.Validators.Dual.JsonDual m s e (Variant (i ∷ Int, s ∷ String, u ∷ Unit))
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


suite :: TestSuite
suite =
  Test.Unit.suite "Test.Json.Validators.Duals" $ do

    Test.Unit.suite "record handling" $ do
      let
        obj ∷ ∀ e. JsonDual Aff Identity e { foo ∷ Int, bar ∷ String, baz ∷ Number }
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
          sumD ∷ JsonDual Aff Identity () Sum
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
          errs =
            [ inj _json { msg: "number is not a string", path: List.Nil }
            , inj _json { msg: "Incorrect tag: S", path: List.Nil }
            , inj _json { msg: "Incorrect tag: S", path: List.Nil }
            , inj _json { msg: "Incorrect tag: S", path: List.Nil }
            , inj _json { msg: "Incorrect tag: S", path: List.Nil }
            , inj _json { msg: "Incorrect tag: S", path: List.Nil }
            ]
        parsedS' ← runValidator (parser sumD) s'

        unV
          (_ `equal` errs)
          (const $ failure "Expecting validation failure")
          parsedS'
