module Test.Polyform.UrlEncoded where

import Prelude
import Data.Map (fromFoldable) as Map
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Polyform (Dual(..))
import Polyform.Batteries.UrlEncoded (Decoded(..))
import Polyform.Batteries.UrlEncoded.Duals (required)
import Polyform.Dual ((~))
import Polyform.Reporter.Dual (liftValidatorDualWith, runReporter)
import Test.Unit (TestSuite)
import Test.Unit (suite, test) as Test.Unit
import Test.Unit.Assert (equal) as Assert

addToCart ∷ Decoded
addToCart = Decoded $ Map.fromFoldable [ "productId" /\ [ "8" ], "amount" /\ [ "9" ], "variety" /\ [ "Reinette" ] ]

suite :: TestSuite
suite = do
  Test.Unit.suite "Test.Polyform.Batteries.UrlEncoded.Duals"
    $ do
        Test.Unit.suite "Error accumulation"
          $ do
              Test.Unit.test " object"
                $ do
                    let
                      -- | Let's create a smiple form layout (`Array [String]`)
                      field name = liftValidatorDualWith (const $ [ name ]) (const $ [ name ]) (required name identity)

                      form = Dual $ { foo: _, bar: _ } <$> _.foo ~ field "foo" <*> _.bar ~ field "bar"

                      -- | We expect that our layout is constructed correctly all the time
                      expected = [ "foo", "bar" ]
                    do
                      let
                        emptyPayload = Decoded $ Map.fromFoldable []
                      Tuple _ given ← runReporter form emptyPayload
                      Assert.equal given expected
                    do
                      let
                        onlyFoo = Decoded $ Map.fromFoldable [ "foo" /\ [ "value" ] ]
                      Tuple _ given ← runReporter form onlyFoo
                      Assert.equal given expected
                    do
                      let
                        both = Decoded $ Map.fromFoldable [ "foo" /\ [ "value" ], "bar" /\ [ "value" ] ]
                      Tuple _ given ← runReporter form both
                      Assert.equal given expected
