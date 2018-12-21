module Test.Main where

import Prelude

import Data.Argonaut (fromNumber, fromObject, fromString)
import Data.Int (toNumber)
import Data.Map as Map
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (unV)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object (fromFoldable) as Object
import Polyform.Dual (Dual(..), (>-))
import Polyform.Dual.Validator as Dual.Validator
import Polyform.Dual.Validators.Json (JsonDual, ObjectDual, (:=))
import Polyform.Dual.Validators.Json (int, number, object, string) as Dual.Json
import Polyform.Dual.Validators.UrlEncoded as Dual.Validators.UrlEncoded
import Polyform.Validator (runValidator)
import Polyform.Validators.Json (JsonError)
import Polyform.Validators.UrlEncoded as UrlEncoded
import Test.Unit (failure, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

d :: forall e m. Monad m => ObjectDual m e { foo :: Int, bar :: String, baz :: Number }
d = Dual $ { foo: _, bar: _, baz: _ }
  <$> (SProxy :: SProxy "foo") := Dual.Json.int
  <*> (SProxy :: SProxy "bar") := Dual.Json.string
  <*> (SProxy :: SProxy "baz") := Dual.Json.number

obj :: forall e. JsonDual
  Aff
  (JsonError + e)
  { foo :: Int
  , bar :: String
  , baz :: Number
  }
obj = Dual.Json.object >>> d

-- dualUrlEncoded :: forall e m. Monad m => Dual.Validators.UrlEncoded.Dual m e { foo :: Int, bar :: String, baz :: Number }
dualUrlEncoded = (Dual $ { foo: _, bar: _, baz: _ }
  <$> _.foo >- Dual.Validators.UrlEncoded.field "foo" Dual.Validators.UrlEncoded.int
  <*> _.bar >- Dual.Validators.UrlEncoded.field "bar" Dual.Validators.UrlEncoded.string
  <*> _.baz >- Dual.Validators.UrlEncoded.field "baz" Dual.Validators.UrlEncoded.number)
  <<< Dual.Validators.UrlEncoded.query { replacePlus: false }

main :: Effect Unit
main = runTest $ do
  Test.Unit.suite "Validators.Urlencoded" $ do
    test "decodes plus to space if option set" $ do
      x <- runValidator (UrlEncoded.query { replacePlus: true }) "field1=some+text+with+spaces"
      unV
        (const $ failure "Validation failed")
        (_ `equal` (Map.fromFoldable [Tuple "field1" ["some text with spaces"]]))
        x
    test "decodes plus as plus to space if option is unset" $ do
      x <- runValidator (UrlEncoded.query { replacePlus: false }) "field1=some+text+with+spaces"
      unV
        (const $ failure "Validation failed")
        (_ `equal` (Map.fromFoldable [Tuple "field1" ["some+text+with+spaces"]]))
        x
    test "decodes repeated value into array" $ do
      x <- runValidator (UrlEncoded.query { replacePlus: false }) "arr=v1&arr=v2&arr=v3"
      unV
        (const $ failure "Validation failed")
        (_ `equal` (Map.fromFoldable [Tuple "arr" ["v1", "v2", "v3"]]))
        x

    test "decodes fields" $ do
      let
        fields = { string: _, int: _, number: _, array: _, boolean: _ }
          <$> UrlEncoded.field "string" UrlEncoded.string
          <*> UrlEncoded.field "int" UrlEncoded.int
          <*> UrlEncoded.field "number" UrlEncoded.number
          <*> UrlEncoded.field "array" UrlEncoded.array
          <*> UrlEncoded.field "boolean" UrlEncoded.boolean

      x <- runValidator
        (UrlEncoded.query { replacePlus: true } >>> fields)
        "string=some+text&int=8&number=0.1&array=v1&array=v2&array=v3&boolean=on"

      unV
        (const $ failure "Validation failed")
        (_ `equal` {string: "some text", int: 8, number: 0.1, array: ["v1", "v2", "v3"], boolean: true})
        x

  Test.Unit.suite "Dual" $ do
    Test.Unit.suite "Json" $ do
      test "serialization / validation" $ do
        let
          input = { foo: 8, bar: "test", baz: 8.0 }
          serialized = Dual.Validator.runSerializer obj input
        let
          xObj = fromObject $ Object.fromFoldable ["foo" /\ fromNumber (toNumber 8), "bar" /\ fromString "test", "baz" /\ fromNumber 8.0]
        parsed <- Dual.Validator.runValidator obj xObj
        let r = serialized == xObj
        assert "Jsons are not equal" r
        unV
          (const $ failure "Validation failed")
          (_ `equal` input)
          parsed

    Test.Unit.suite "Urlencoded" $ do
      test "serialization / validation" $ do
        let
          input = { foo: 8, bar: "test", baz: 8.2 }
        serialized ← Dual.Validator.runSerializer' dualUrlEncoded input
        traceM serialized
        let
          qStr = Dual.Validators.UrlEncoded.Decoded $ Map.fromFoldable ["foo" /\ ["8"], "bar" /\ ["test"], "baz" /\ ["8.0"]]
        parsed <- Dual.Validator.runValidator dualUrlEncoded "foo=8&bar=test&baz=8.2"
        traceM parsed
        -- assert "Urlencoded maps are not equal" (serialized == qStr)
        -- unV
        --   (const $ failure "Validation failed")
        --   (_ `equal` input)
        --   parsed
