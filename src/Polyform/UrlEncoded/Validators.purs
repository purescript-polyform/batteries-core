-- | This module provides validators for urlencoded values.
-- | In general it follows "browsers standard" for encoding
-- | so it should be useful in the context of HTML form validation.
-- |
module Polyform.UrlEncoded.Validators
  ( Error
  , FieldError
  , _booleanExpected
  , _fieldError
  , _intExpected
  , _nonBlankExpected
  , _numberExpected
  , _queryParseError
  , _singleStringExpected
  , array
  , boolean
  , field
  , int
  , module Query
  , nonBlank
  , number
  , optional
  , query
  , singleString
  )
  where

import Prelude

import Data.Bifunctor (lmap)
import Data.Int as Int
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Variant (Variant, inj)
import Polyform.UrlEncoded.Query (Decoded(..), Key, Options, Value, parse) as Query
import Polyform.Validator (check, hoistFnMaybe) as Validator
import Polyform.Validator (hoistFn, hoistFnMV, hoistFnV, runValidator)
import Polyform.Validators (Validator, error, invalid) as Validators
import Type.Prelude (SProxy(..))

type Field m e b = Validators.Validator m e (Maybe Query.Value) b

type FieldError fieldErrs = { name ∷ String, errors ∷ Array (Variant fieldErrs) }

type Error fieldErrs errs =
  ( urlEncodedQueryParseError ∷ String
  , urlEncodedFieldError ∷ FieldError fieldErrs
  | errs
  )

_queryParseError = SProxy ∷ SProxy "urlEncodedQueryParseError"

query ∷ ∀ m e errs. Monad m ⇒ Query.Options → Validators.Validator m (Error e errs) String Query.Decoded
query opts = Validator.hoistFnMaybe (Validators.error _queryParseError) (Query.parse opts)

_fieldError = SProxy ∷ SProxy "urlEncodedFieldError"

field ∷ ∀ a e m errs. Monad m ⇒ Query.Key → Field m e a → Validators.Validator m (Error e errs) Query.Decoded a
field name validator =
  hoistFnMV $ \(Query.Decoded q) → do
    let input = Map.lookup name q
    result ← runValidator validator input
    pure $ lmap failure result
  where
    failure errors = [ inj _fieldError { errors, name } ]

optional ∷ ∀ a e m. Monad m ⇒ Field m e a → Field m e (Maybe a)
optional fv = hoistFnMV $ case _ of
  Just [] → pure (pure Nothing)
  Nothing → pure (pure Nothing)
  value → map Just <$> runValidator fv value

-- | Encodes default browser behavior which sets `checkbox` value to "on"
-- | when checked and skips it completely when it is not.
-- | We consider also "off" value because we want to be more consistent when
-- | building API comunication layer - if you have any objections please fill
-- | an issue with description.

_booleanExpected = SProxy ∷ SProxy "urlEncodedBooleanExpected"

boolean ∷ ∀ e m. Applicative m ⇒ Field m (urlEncodedBooleanExpected ∷ Query.Value | e) Boolean
boolean = hoistFnV case _ of
  Just ["on"] → pure true
  Just ["off"] → pure false
  Nothing → pure false
  Just v → Validators.invalid _booleanExpected v

_singleStringExpected = SProxy ∷ SProxy "urlEncodedSingleStringExpected"

singleString ∷ ∀ e m. Applicative m ⇒ Field m (urlEncodedSingleStringExpected ∷ Maybe Query.Value | e) String
singleString = hoistFnV $ case _ of
  Just [v] → pure v
  v → Validators.invalid _singleStringExpected v

_nonBlankExpected = SProxy ∷ SProxy "urlEncodedNonBlankExpected"

nonBlank ∷ ∀ e m. Monad m ⇒ Field m (urlEncodedNonBlankExpected ∷ Unit, urlEncodedSingleStringExpected ∷ Maybe Query.Value | e) String
nonBlank = singleString >>> Validator.check (const $ Validators.error _nonBlankExpected unit) (not <<< eq "")

_numberExpected = SProxy ∷ SProxy "urlEncodedNumberExpected"

number ∷ ∀ e m. Monad m ⇒ Field m (urlEncodedSingleStringExpected ∷ Maybe Query.Value, urlEncodedNumberExpected ∷ String | e) Number
number = singleString >>> Validator.hoistFnMaybe (Validators.error _numberExpected) Number.fromString

_intExpected = SProxy ∷ SProxy "urlEncodedIntExpected"

int ∷ ∀ e m. Monad m ⇒ Field m (urlEncodedSingleStringExpected ∷ Maybe Query.Value, urlEncodedIntExpected ∷ String | e) Int
int = singleString >>> Validator.hoistFnMaybe (Validators.error _intExpected) Int.fromString

array ∷ ∀ e m. Monad m ⇒ Field m e (Array String)
array = hoistFn $ case _ of
  Just s → s
  Nothing → []

