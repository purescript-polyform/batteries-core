module Validators.Json 
  ( JsError
  , JsValidation
  , array
  , arrayOf
  , elem
  , field
  , int
  , object
  , optionalField
  , string
  ) where

import Prelude

import Data.Argonaut (Json, foldJson, toArray, toNumber, toObject, toString)
import Data.Array (fromFoldable, singleton, (!!))
import Data.Bifunctor (lmap)
import Data.Int (fromNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.StrMap (StrMap, lookup)
import Data.Traversable (sequence, traverse)
import Polyform.Validation (V(..), Validation, hoistFnMV, hoistFnV, runValidation)

data JsError = JsError (List String) String

instance showJsError :: Show JsError where
  show (JsError path msg) = "(Error at" <> show (fromFoldable path) <> " " <> msg <> ")"

type JsValidation m a = Validation m (Array JsError) Json a

jsType :: Json -> String
jsType = foldJson
  (const "null")
  (const "bool")
  (const "number")
  (const "string")
  (const "array")
  (const "object")

fail :: forall a. String -> V (Array JsError) a
fail msg = Invalid $ singleton $ JsError Nil msg

int :: forall m. Monad m => JsValidation m Int
int = hoistFnV $ \v ->
  case toNumber v >>= fromNumber of
    Nothing -> fail (jsType v <> " is not an int")
    Just n -> pure n

object :: forall m. Monad m => Validation m (Array JsError) Json (StrMap Json)
object = hoistFnV $ \v ->
  case toObject v of
    Nothing -> fail (jsType v <> " is not an object")
    Just o -> pure o

string :: forall m. Monad m => JsValidation m String
string = hoistFnV $ \v ->
  case toString v of
    Nothing -> fail (jsType v <> " is not a string")
    Just s -> pure s

field :: forall m a. Monad m => String -> JsValidation m a -> JsValidation m a
field f nested = object >>> hoistFnMV (\v ->
  case lookup f v of
    Nothing -> pure $ fail ("no field " <> show f <> " in object " <> show v)
    Just json -> do
      res <- runValidation nested json
      pure $ lmap (map \(JsError p e) -> JsError (f : p) e) res)

optionalField
  :: forall m a
   . Monad m
  => Monoid a
  => String -> JsValidation m a -> JsValidation m a
optionalField f nested = object >>> hoistFnMV (\v ->
  case lookup f v of
    Nothing -> pure $ pure mempty
    Just json -> do
      res <- runValidation nested json
      pure $ lmap (map \(JsError p e) -> JsError (f : p) e) res)

array :: forall m. Monad m => JsValidation m (Array Json)
array = hoistFnV $ \v ->
  case toArray v of
    Nothing -> fail (jsType v <> " is not an array")
    Just a -> pure a

elem :: forall m a. Monad m => Int -> JsValidation m a -> JsValidation m a
elem i v = array >>> hoistFnMV (\arr ->
  case arr !! i of
    Nothing -> pure $ fail ("no element at index " <> show i)
    Just a -> runValidation v a)

arrayOf :: forall m a. Monad m => JsValidation m a -> JsValidation m (Array a)
arrayOf v = array >>> hoistFnMV f
  where
    f = map sequence <<< traverse (runValidation v)
