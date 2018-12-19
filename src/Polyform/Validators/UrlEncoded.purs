module Polyform.Validators.UrlEncoded
  ( module Types
  , FieldValueValidator
  , array
  , boolean
  , field
  , int
  , number
  , optional
  , parse
  , string
  )
  where

import Prelude

import Data.Array (singleton) as Array
import Data.Bifunctor (lmap)
import Data.Int as Int
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Validation.Semigroup (invalid)
import Data.Variant (inj)
import Polyform.Validator (hoistFn, hoistFnEither, hoistFnMV, hoistFnV, runValidator)
import Polyform.Validator as Polyform.Validator
import Polyform.Validators (Errors)
import Polyform.Validators.UrlEncoded.Parser (Options, parse) as Parser
import Polyform.Validators.UrlEncoded.Types (Decoded, Error, Validator, _urlDecoding, _urlField)
import Polyform.Validators.UrlEncoded.Types (Decoded, Error, Validator, _urlDecoding, _urlField) as Types

-- | This module provides validators for urlencoded values.
-- | In general it follows "browsers standard" for encoding
-- | so it should be useful in the context of request validation.
-- | On the other hand if you want to build backend form
-- | validation solution on top of this it is probably
-- | better to take a look at `Polyform.Validator.Reporter`.

parse :: forall m e. Monad m => Parser.Options -> Validator m e String Decoded
parse opts = hoistFnEither (lmap (Array.singleton <<< inj _urlDecoding) <<< Parser.parse opts)

-- | `String` error is transformed into `Types.Error` in "form" level validators
type FieldValueValidator m a = Polyform.Validator.Validator m String (Maybe (Array String)) a

-- | Encodes default browser behavior which sets `checkbox` value to "on"
-- | when checked and skips it completely when it is not.
-- | We consider also "off" value because we want to be more consistent when
-- | building API comunication layer - if you have any objections please fill
-- | an issue with description.
boolean :: forall m. Monad m => FieldValueValidator m Boolean
boolean = hoistFnV $ case _ of
  Just ["on"] -> pure true
  Just ["off"] -> pure false
  Nothing -> pure false
  Just v -> invalid $ "Could not parse \"" <> show v <> "\" value as boolean."

string :: forall m. Monad m => FieldValueValidator m String
string = hoistFnV $ case _ of
  Just [v] -> pure v
  Just v -> invalid $ "Multiple values provided: " <> show v
  Nothing -> invalid $ "Missing value"

number :: forall m. Monad m => FieldValueValidator m Number
number = flip compose string $ hoistFnV $ \s -> case Number.fromString s of
  Just n -> pure n
  Nothing -> invalid $ "Could not parse " <> s <> " as number"

int :: forall m. Monad m => FieldValueValidator m Int
int = flip compose string $ hoistFnV $ \s -> case Int.fromString s of
  Just n -> pure n
  Nothing -> invalid $ "Could not parse " <> s <> " as int"

array :: forall m. Monad m => FieldValueValidator m (Array String)
array = hoistFn $ case _ of
  Just s -> s
  Nothing -> []

optional :: forall a m. Monad m => FieldValueValidator m a -> FieldValueValidator m (Maybe a)
optional v = hoistFnMV $ case _ of
  Just [] -> pure (pure Nothing)
  Nothing -> pure (pure Nothing)
  value -> map Just <$> runValidator v value

field :: forall a e m. Monad m => String -> FieldValueValidator m a -> Validator m e Decoded a
field name validator =
  hoistFnMV (Map.lookup name >>> pure >=> runValidator validator >=> lmap failure >>> pure)
  where
  failure :: String -> Errors (Error e)
  failure e = [ inj _urlField { field: name, error: e } ]

