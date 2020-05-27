module Test.Polyform.Decimal where

import Prelude

import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Argonaut (fromNumber, fromObject, fromString)
import Data.Array (all)
import Data.Decimal (Decimal)
import Data.Decimal (fromString) as Decimal
import Data.Either (Either)
import Data.Enum (class Enum, enumFromTo)
import Data.Functor.Compose (Compose(..))
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromJust)
import Data.Profunctor.Star (Star(..))
import Data.Tuple.Nested ((/\))
import Data.Validation.Semigroup (V(..))
import Data.Validation.Semigroup (unV)
import Effect (Effect)
import Foreign.Object (fromFoldable) as Object
import Partial.Unsafe (unsafePartial)
import Polyform.Decimal (formatting, parse, print) as Decimal
import Polyform.Decimal (formatting, validator)
import Record.Extra (sequenceRecord)
import Test.QuickCheck (class Arbitrary, class Coarbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Laws (A, B, C, checkLaws)
import Test.QuickCheck.Laws.Control as Control
import Test.QuickCheck.Laws.Data as Data
import Test.Unit (TestSuite, failure, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (equal)
import Test.Unit.Assert (equal) as Assert
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy2(..), Proxy3(..))
import Unsafe.Coerce (unsafeCoerce)

newtype ADecimal = ADecimal Decimal
derive instance eqADecimal ∷ Eq ADecimal

instance arbitraryADecimal ∷ Arbitrary ADecimal where
  arbitrary =
    -- | Type annotation just for readability
    let
      intPart = arbitrary ∷ Gen Int
      decimalPart = arbitrary ∷ Gen Int
      decimal i d = ADecimal $ unsafePartial $ fromJust <<< Decimal.fromString $ (show i <> "." <> show d)
    in
      decimal <$> intPart <*> decimalPart

suite ∷ TestSuite
suite =
  Test.Unit.suite "Test.Polyform.Decimal" $ do
    Test.Unit.suite "formatting with dot separator and comma decimal separator" $ do
      let
        formatting = Decimal.formatting { decimalSeparator: Just ",", separators: [".", " "] }
      test "print" $ do
        Assert.equal (Just "1.234.567,9889") (Decimal.fromString "1234567.9889" >>= Decimal.print formatting >>> pure)

      test "parse with space separator" $ do
        Assert.equal (Decimal.fromString "1234567.9889") (Decimal.parse formatting "1 234 567,9889")

      test "parse with dot separator" $ do
        Assert.equal (Decimal.fromString "1234567.9889") (Decimal.parse formatting "1.234.567,9889")

      test "parse with mixed separator" $ do
        Assert.equal (Decimal.fromString "1234567.9889") (Decimal.parse formatting "1.234 567,9889")


    Test.Unit.suite "formatting with comma separator and dot decimal separator" $ do
      let
        formatting = Decimal.formatting { decimalSeparator: Just ".", separators: [",", " "] }

      test "print" $ do
        Assert.equal (Just "1,234,567.9889") (Decimal.fromString "1234567.9889" >>= Decimal.print formatting >>> pure)

      test "parse with space separator" $ do
        Assert.equal (Decimal.fromString "1234567.9889") (Decimal.parse formatting "1,234,567.9889")

      test "parse with dot separator" $ do
        Assert.equal (Decimal.fromString "1234567.9889") (Decimal.parse formatting "1 234,567.9889")

      test "parse decimal with only fractional part" $ do
        Assert.equal (Decimal.fromString "0.9889") (Decimal.parse formatting "0.9889")

      test "parse decimal with only integer part" $ do
        Assert.equal (Decimal.fromString "1234567") (Decimal.parse formatting "1 234,567")
