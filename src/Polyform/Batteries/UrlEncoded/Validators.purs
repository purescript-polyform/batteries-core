-- | This module provides validators for urlencoded values.
-- | In general it follows "browsers standard" for encoding
-- | so it should be useful in the context of HTML form validation.
-- |
module Polyform.Batteries.UrlEncoded.Validators
  ( _booleanExpected
  , _intExpected
  , _nonBlankExpected
  , _numberExpected
  , _singleStringExpected
  , array
  , boolean
  , field
  , int
  , module Query
  , nonBlank
  , number
  , optionalField
  , singleString
  )
  where

import Prelude

import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Polyform.Batteries (Validator, error, invalid) as Batteries
import Polyform.Batteries.UrlEncoded.Query (Decoded(..), Key, Value, lookup) as Query
import Polyform.Batteries.UrlEncoded.Types (Validator, fromValidator)
import Polyform.Validator (check, liftFn, liftFnMaybe) as Validator
import Polyform.Validator (liftFn, liftFnMV, liftFnV, runValidator)
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

type Field m e b = Batteries.Validator m e (Maybe Query.Value) b

field ∷ ∀ a m errs. Monad m ⇒ Query.Key → Field m errs a → Validator m errs Query.Decoded a
field name fieldValidator = fromValidator name (fieldValidator <<< Validator.liftFn (Query.lookup name))

optionalField ∷ ∀ a e m. Monad m ⇒ Field m e a → Field m e (Maybe a)
optionalField fv = liftFnMV $ case _ of
  Just [] → pure (pure Nothing)
  Nothing → pure (pure Nothing)
  value → map Just <$> runValidator fv value

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

_singleStringExpected = SProxy ∷ SProxy "singleStringExpected"

type SingleStringExpected e = (singleStringExpected ∷ Maybe Query.Value | e)

singleString ∷ ∀ e m. Applicative m ⇒ Field m (SingleStringExpected + e) String
singleString = liftFnV $ case _ of
  Just [v] → pure v
  v → Batteries.invalid _singleStringExpected v

_nonBlankExpected = SProxy ∷ SProxy "nonBlankExpected"

nonBlank ∷ ∀ e m. Monad m ⇒ Field m (nonBlankExpected ∷ Unit, singleStringExpected ∷ Maybe Query.Value | e) String
nonBlank = singleString >>> Validator.check (const $ Batteries.error _nonBlankExpected unit) (not <<< eq "")

_numberExpected = SProxy ∷ SProxy "numberExpected"

number ∷ ∀ e m. Monad m ⇒ Field m (singleStringExpected ∷ Maybe Query.Value, numberExpected ∷ String | e) Number
number = singleString >>> Validator.liftFnMaybe (Batteries.error _numberExpected) Number.fromString

_intExpected = SProxy ∷ SProxy "intExpected"

int ∷ ∀ e m. Monad m ⇒ Field m (singleStringExpected ∷ Maybe Query.Value, intExpected ∷ String | e) Int
int = singleString >>> Validator.liftFnMaybe (Batteries.error _intExpected) Int.fromString

array ∷ ∀ e m. Monad m ⇒ Field m e (Array String)
array = liftFn $ case _ of
  Just s → s
  Nothing → []

