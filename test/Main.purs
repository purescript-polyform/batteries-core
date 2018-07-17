module Test.Main where

import Prelude

import Data.Tuple (Tuple(..))
import Effect (Effect)
import Foreign.Object (empty, fromFoldable)
import Polyform.Validation (V(..), runValidation)
import Polyform.Validators.UrlEncoded (urlEncoded)
import Test.Unit (failure, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (equal)
import Test.Unit.Main (runTest)


-- main :: Effect Unit
main :: Effect Unit
main = runTest $ do
    Test.Unit.suite "Urlencoded" $ do
      test "decodes plus to space if option set" $ do
        x ← runValidation (urlEncoded { replacePlus: true }) "field1=some+text+with+spaces"
        case x of
          Valid _ x' → equal x' (fromFoldable [Tuple "field1" ["some text with spaces"]])
          Invalid _ → failure "Validation failed"
      test "decodes plus as plus to space if option is unset" $ do
        x ← runValidation (urlEncoded { replacePlus: false }) "field1=some+text+with+spaces"
        case x of
          Valid _ x' → equal x' (fromFoldable [Tuple "field1" ["some+text+with+spaces"]])
          Invalid _ → failure "Validation failed"
