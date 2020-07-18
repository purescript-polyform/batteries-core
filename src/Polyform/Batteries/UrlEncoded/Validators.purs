-- | This module provides validators for urlencoded values.
-- | In general it follows "browsers standard" for encoding
-- | so it should be useful in the context of HTML form validation.
-- |
module Polyform.Batteries.UrlEncoded.Validators
  ( BooleanExpected
  , Field
  , IntExpected
  , MissingValue
  , SingleField
  , module Query
  , _booleanExpected
  , _intExpected
  , _missingValue
  , array
  , boolean
  , optional
  , optValidator
  , required
  , int
  , number
  , value
  )
  where

import Prelude

import Data.Array (head) as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Validation.Semigroup (V(..))
import Polyform.Batteries (Validator, error, invalid) as Batteries
import Polyform.Batteries.Number (NumberExpected)
import Polyform.Batteries.Number (validator) as Batteries.Number
import Polyform.Batteries.UrlEncoded.Query (Decoded(..), Key, Value, lookup) as Query
import Polyform.Batteries.UrlEncoded.Types (Validator, fromValidator)
import Polyform.Validator (liftFn, liftFnMV, liftFnV, runValidator)
import Polyform.Validator (liftFn, liftFnMaybe) as Validator
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

type Field m e b = Batteries.Validator m e (Maybe Query.Value) b

type SingleField m e b = Batteries.Validator m e String b
type MultiField m e b = Batteries.Validator m e (Array String) b

required
  ∷ ∀ a m errs
  . Monad m
  ⇒ Query.Key
  → SingleField m (MissingValue + errs) a
  → Validator m (MissingValue + errs) Query.Decoded a
required name fieldValidator =
  fromValidator
    name
    (fieldValidator <<< value <<< Validator.liftFn (Query.lookup name))

optional
  ∷ ∀ a m errs
  . Monad m
  ⇒ Query.Key
  → SingleField m (errs) a
  → Validator m (errs) Query.Decoded (Maybe a)
optional name fieldValidator =
  fromValidator name (optValidator fieldValidator <<< Validator.liftFn (Query.lookup name))

_missingValue = SProxy ∷ SProxy "missingValue"

type MissingValue e = (missingValue ∷ Unit | e)

value ∷ ∀ e m. Applicative m ⇒ Field m (MissingValue + e) String
value = liftFnV $ \qv → case qv >>= Array.head of
    Just "" → Batteries.invalid _missingValue unit
    Just v → pure v
    Nothing → Batteries.invalid _missingValue unit

-- | We could do a bit of dance with `Choice.first` etc.
-- | but this seems simpler and a bit more efficient
optValidator ∷ ∀ b e m. Monad m ⇒ SingleField m e b → Field m e (Maybe b)
optValidator fieldValidator = liftFnMV \v → case v >>= Array.head of
  Nothing → pure (V (Right Nothing))
  Just "" → pure (V (Right Nothing))
  Just h → runValidator (Just <$> fieldValidator) h

-- | Encodes default browser behavior which sets `checkbox` value to "on"
-- | when checked and skips it completely when it is not.
-- | We consider also "off" value because we want to be more consistent when
-- | building API comunication layer - if you have any objections please fill
-- | an issue with description.

_booleanExpected = SProxy ∷ SProxy "booleanExpected"

type BooleanExpected e = (booleanExpected ∷ Query.Value | e)

boolean ∷ ∀ e m. Applicative m ⇒ Field m (booleanExpected ∷ Query.Value | e) Boolean
boolean = liftFnV case _ of
  Just ["on"] → pure true
  Just ["off"] → pure false
  Nothing → pure false
  Just v → Batteries.invalid _booleanExpected v

number ∷ ∀ e m. Monad m ⇒ SingleField m (NumberExpected + e) Number
number = Batteries.Number.validator

_intExpected = SProxy ∷ SProxy "intExpected"

type IntExpected e = (intExpected ∷ String | e)

int ∷ ∀ e m. Monad m ⇒ SingleField m (IntExpected + e) Int
int = Validator.liftFnMaybe (Batteries.error _intExpected) Int.fromString

array ∷ ∀ e m. Monad m ⇒ Field m e (Array String)
array = liftFn $ case _ of
  Just s → s
  Nothing → []

