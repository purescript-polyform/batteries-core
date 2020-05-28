module Test.Polyform.Batteries.Decimal where

import Prelude

import Data.Decimal (Decimal)
import Data.Decimal (fromString) as Decimal
import Data.Maybe (Maybe(..), fromJust)
import Partial.Unsafe (unsafePartial)
import Polyform.Batteries.Decimal (formatting, parse, print) as Decimal
import Test.QuickCheck (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.Unit (TestSuite, test)
import Test.Unit (suite) as Test.Unit
import Test.Unit.Assert (equal) as Assert

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
