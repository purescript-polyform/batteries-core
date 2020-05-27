module Polyform.Decimal where

import Prelude

import Data.Array (catMaybes, uncons) as Array
import Data.Array.NonEmpty (toArray) as NonEmptyArray
import Data.Decimal (Decimal)
import Data.Decimal (fromString, toString) as Decimal
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), joinWith, split) as String
import Data.String.Regex (match, replace) as Regex
import Data.String.Regex.Flags (global) as Regex.Flags
import Data.String.Regex.Unsafe (unsafeRegex)
import Polyform.Data.String (reverseCodeUnits) as String
import Polyform.Data.String.Regex (escape) as Regex
import Polyform.Dual (dual) as Dual
import Polyform.Validator (hoistFnMaybe) as Validator
import Polyform.Validators (Validator, Dual) as Validators
import Polyform.Validators (error) as Polyform.Validtors
import Type.Prelude (SProxy(..))

newtype Formatting = Formatting
  { parse ∷ String → Maybe Decimal
  , print ∷ Decimal → String
  }

formatting ∷ { decimalSeparator ∷ Maybe String, separators ∷ Array String } → Formatting
formatting config = Formatting
  { parse: parse' config
  , print: print' config
  }
  where
    parse' ∷ { decimalSeparator ∷ Maybe String, separators ∷ Array String } → String → Maybe Decimal
    parse' { decimalSeparator, separators } =
      let
        dropSeparators = case map Regex.escape separators of
          [] → identity
          separators' →
            let
              regex = unsafeRegex ("(" <> String.joinWith "|" separators' <> ")") Regex.Flags.global
            in
              Regex.replace regex ""
        replaceDecimalSeparator = fromMaybe identity do
          s ← decimalSeparator
          let
            regex = unsafeRegex (Regex.escape s) mempty
          pure $ Regex.replace regex "."
      in
        Decimal.fromString <<< replaceDecimalSeparator <<< dropSeparators

    print' ∷ { decimalSeparator ∷ Maybe String, separators ∷ Array String } → Decimal → String
    print' { decimalSeparator, separators } =
      let
        decimalSeparator' = fromMaybe "." decimalSeparator

        format s = case Array.uncons separators, String.split (String.Pattern ".") s of
          Just { head: sep }, [ integerPart, fractionalPart ] →
            insertSep integerPart sep <> decimalSeparator' <> fractionalPart
          Nothing, [ integerPart, fractionalPart ] → integerPart <> decimalSeparator' <> fractionalPart
          Just { head: sep }, _  → insertSep sep s
          _, _ → s

        triplets = Regex.match (unsafeRegex "([0-9]{1,3})" (Regex.Flags.global))

        -- | Spiting reversed digit string into triplets
        -- | join with separator and reverse it back.
        insertSep ∷ String → String → String
        insertSep s sep
          = String.reverseCodeUnits
          <<< String.joinWith sep
          <<< maybe [] (Array.catMaybes <<< NonEmptyArray.toArray)
          <<< triplets
          <<< String.reverseCodeUnits
          $ s
      in
        format <<< Decimal.toString

_decimal = SProxy ∷ SProxy "decimal"

parse ∷ Formatting → String → Maybe Decimal
parse (Formatting fmt) = fmt.parse

print ∷ Formatting → Decimal → String
print (Formatting fmt) = fmt.print

validator
  ∷ ∀ e m
  . Applicative m
  ⇒ Formatting
  → Validators.Validator m (decimal ∷ String | e) String Decimal
validator (Formatting { parse: p }) =
  Validator.hoistFnMaybe (Polyform.Validtors.error _decimal) p

dual
  ∷ ∀ e m s
  . Applicative m
  ⇒ Applicative s
  ⇒ Formatting
  → Validators.Dual m s (decimal ∷ String | e) String Decimal
dual (fmt@(Formatting { print: p })) =
  Dual.dual (validator fmt) (p >>> pure)


-- | Usually you want to push formatting context into the validator
-- | monad to simplify form creation and performance benefits.
-- |
-- | ```purescript
-- | validatorReader
-- |   ∷ ∀ ctx errs m polyformCtx
-- |   . MonadAsk { polyform ∷ { decimal ∷ Formatting | polyformCtx } | ctx } m
-- |   ⇒ Validators.Validator
-- |     m
-- |     ( decimal ∷ String | errs )
-- |     String
-- |     Decimal
-- | validatorReader = Validator.hoistFnMV \i → do
-- |   fmt ← asks _.polyform.decimal
-- |   Validator.runValidator (validator fmt) i
-- | ```
--
-- | The same goes for `Dual` but in this case we add decimal formatting into
-- | the serializer context as well:
-- |
-- | ```purescript
-- | dualReader
-- |   ∷ ∀ ctx errs m polyformCtx s
-- |   . MonadAsk { polyform ∷ { decimal ∷ Formatting | polyformCtx } | ctx } m
-- |   ⇒ MonadAsk { polyform ∷ { decimal ∷ Formatting | polyformCtx } | ctx } s
-- |   ⇒ Validators.Dual
-- |     m
-- |     s
-- |     ( decimal ∷ String | errs )
-- |     String
-- |     Decimal
-- | dualReader = Dual.dual v (\d → asks _.polyform.decimal >>= flip print d >>> pure)
-- |   where
-- |     v = Validator.hoistFnMV \i → do
-- |       fmt ← asks _.polyform.decimal
-- |       Validator.runValidator (validator fmt) i
-- | ```
