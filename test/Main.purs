module Test.Main where

import Prelude

import Data.Argonaut (fromNumber, fromObject, fromString)
import Data.Int (toNumber)
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (unV)
import Effect (Effect)
import Foreign.Object (fromFoldable)
import Polyform.Validator (runValidator)
import Polyform.Validator.Dual (Dual(..), DualD(..), (>-))
import Polyform.Validators.Dual.Json (JsonDual, ObjectDual, mapD, object, objectField)
import Polyform.Validators.Dual.Json (int, number, string) as Dual.Json
import Polyform.Validators.Json (JsError)
import Polyform.Validators.UrlEncoded (urlEncoded)
import Test.Unit (failure, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)
import Type.Row (type (+))


d :: forall e m.  Monad m => ObjectDual m e { foo :: Int, bar :: String, baz ∷ Number }
d = Dual $ { foo: _, bar: _, baz: _ }
    <$> _.foo >- objectField "foo" Dual.Json.int
    <*> _.bar >- objectField "bar" Dual.Json.string
    <*> _.baz >- objectField "baz" Dual.Json.number

obj :: forall e m
   . Monad m
  => JsonDual
      m
      (JsError + e)
      { foo :: Int
      , bar :: String
      , baz ∷ Number
      }
obj = object >>> mapD wrap unwrap >>> d


main :: Effect Unit
main = runTest $ do
  Test.Unit.suite "Dual" $ do
    test "serialization / validation" $ do
      let
        Dual (DualD { serializer, validator }) = obj
        input = { foo: 8, bar: "test", baz: 8.0 }
        x = serializer { foo: 8, bar: "test", baz: 8.0 }
        xObj = fromObject $ fromFoldable ["foo" /\ fromNumber (toNumber 8), "bar" /\ fromString "test", "baz" /\ fromNumber 8.0]
      j ← runValidator validator x
      let r = x == xObj
      assert "Jsons are not equal" r
      unV
        (const $ failure "Validation failed")
        (_ `equal` input)
        j

      -- traceM x
      -- traceM "TEST"
  Test.Unit.suite "Urlencoded" $ do
    test "decodes plus to space if option set" $ do
      x ← runValidator (urlEncoded { replacePlus: true }) "field1=some+text+with+spaces"
      unV
        (const $ failure "Validation failed")
        (_ `equal` (fromFoldable [Tuple "field1" ["some text with spaces"]]))
        x
    test "decodes plus as plus to space if option is unset" $ do
      x ← runValidator (urlEncoded { replacePlus: false }) "field1=some+text+with+spaces"
      unV
        (const $ failure "Validation failed")
        (_ `equal` (fromFoldable [Tuple "field1" ["some+text+with+spaces"]]))
        x
