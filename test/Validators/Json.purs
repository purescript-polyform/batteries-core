module Test.Validators.Json where

import Prelude

import Data.Argonaut (fromBoolean, fromNumber) as Argonaut
import Data.Argonaut (fromBoolean, fromNumber, fromObject, fromString, stringify, toObject)
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
import Data.Variant (Variant, case_, inj, match, on, onMatch)
import Debug.Trace (traceM)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Foreign.Object (Object, fromFoldable) as Object
import Global.Unsafe (unsafeStringify)
import Polyform.Dual (Dual(..), DualD(..), dual, serializer, (~))
import Polyform.Dual.Generic (sum, variant) as Dual.Generic
import Polyform.Dual.Validators.UrlEncoded as Dual.Validators.UrlEncoded
import Polyform.Validator (hoistFn, hoistFnMV, hoistFnV, runValidator, valid)
import Polyform.Validators.Json (JsonError, Validator, field, int, number, object, string)
import Polyform.Validators.Json (Validator, JsonDecodingError, boolean, failure, field, int, jsType, json, string) as Json
import Polyform.Validators.UrlEncoded as UrlEncoded
import Record.Extra (sequenceRecord)
import Test.Unit (failure, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (assert, equal)
import Test.Unit.Main (runTest)
import Type.Prelude (SProxy(..), reflectSymbol)
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

obj ∷ ∀ e m. Monad m ⇒ Validator m e { foo ∷ Int, bar ∷ String, baz ∷ Number }
obj = object >>> d
  where
    d = sequenceRecord
      { foo: field "foo" int
      , bar: field "bar" string
      , baz: field "baz" number
      }

suite =
  Test.Unit.suite "Test.Validators.Json" $ do
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
