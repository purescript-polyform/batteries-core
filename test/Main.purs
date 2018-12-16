module Test.Main where

import Prelude

import Data.Argonaut (fromNumber, fromObject, fromString)
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (unV)
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign.Object (fromFoldable)
import Polyform.Dual (Dual(..))
import Polyform.Dual.Validator as Dual.Validator
import Polyform.Dual.Validators.Json (JsonDual, ObjectDual, (:=))
import Polyform.Dual.Validators.Json (int, number, object, string) as Dual.Json
import Polyform.Validator (runValidator)
import Polyform.Validators.Json (JsError)
import Polyform.Validators.UrlEncoded (urlEncoded)
import Test.Unit (failure, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

d :: forall e m. Monad m => ObjectDual m e { foo :: Int, bar :: String, baz :: Number }
d = Dual $ { foo: _, bar: _, baz: _ }
  <$> (SProxy ∷ SProxy "foo") := Dual.Json.int
  <*> (SProxy ∷ SProxy "bar") := Dual.Json.string
  <*> (SProxy ∷ SProxy "baz") := Dual.Json.number

obj :: forall e. JsonDual
  Aff
  (JsError + e)
  { foo :: Int
  , bar :: String
  , baz :: Number
  }
obj = Dual.Json.object >>> d

main :: Effect Unit
main = runTest $ do
  Test.Unit.suite "Dual" $ do
    test "serialization / validation" $ do
      let
        input = { foo: 8, bar: "test", baz: 8.0 }
        serialized = Dual.Validator.runSerializer obj input
      let
        xObj = fromObject $ fromFoldable ["foo" /\ fromNumber (toNumber 8), "bar" /\ fromString "test", "baz" /\ fromNumber 8.0]
      parsed <- Dual.Validator.runValidator obj xObj
      let r = serialized == xObj
      assert "Jsons are not equal" r
      pure unit
      unV
        (const $ failure "Validation failed")
        (_ `equal` input)
        parsed

  Test.Unit.suite "Urlencoded" $ do
    test "decodes plus to space if option set" $ do
      x <- runValidator (urlEncoded { replacePlus: true }) "field1=some+text+with+spaces"
      unV
        (const $ failure "Validation failed")
        (_ `equal` (fromFoldable [Tuple "field1" ["some text with spaces"]]))
        x
    test "decodes plus as plus to space if option is unset" $ do
      x <- runValidator (urlEncoded { replacePlus: false }) "field1=some+text+with+spaces"
      unV
        (const $ failure "Validation failed")
        (_ `equal` (fromFoldable [Tuple "field1" ["some+text+with+spaces"]]))
        x
