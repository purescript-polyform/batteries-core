module Polyform.Validators.Json
  ( JsonError
  , JsonDecodingError
  , JsonValidator
  , array
  , arrayOf
  , elem
  , extendErr
  , failure
  , field
  , number
  , int
  , json
  , object
  , optionalField
  , string
  )
  where

import Prelude

import Data.Argonaut (Json, caseJson, jsonParser, stringify, toArray, toNumber, toObject, toString)
import Data.Array ((!!))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Functor (mapFlipped)
import Data.Int (fromNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, traverse)
import Data.Validation.Semigroup (V, invalid)
import Data.Variant (inj, prj)
import Foreign.Object (Object, lookup)
import Polyform.Validator (hoistFnMV, hoistFnV, runValidator)
import Polyform.Validators (Errors, Validator, fail)

-- | Validator which builds `Json` from `String`.
-- | It is not incorporated into default `JsonValidator` stack
-- | because you don't have to start from `String` value
-- | (as it is in case of for example Affjax).
type JsonDecodingError e = (jsonDecodingError :: String | e)
json
  :: forall e m
   . Monad m
  => Validator
      m
      (JsonDecodingError e)
      String
      Json
json = hoistFnV $ jsonParser >>> case _ of
  Right j -> pure j
  Left e -> invalid ([inj (SProxy :: SProxy "jsonDecodingError") e])

type JsonError r = (jsonError :: { path :: List String, msg :: String } | r)
type JsonValidator m e a = Validator m (JsonError e) Json a

jsType :: Json -> String
jsType = caseJson
  (const "null")
  (const "bool")
  (const "number")
  (const "string")
  (const "array")
  (const "object")

_jsonErr = SProxy :: SProxy "jsonError"

failure :: forall e a. String -> V (Errors (JsonError e)) a
failure msg = fail $ inj _jsonErr { path: Nil, msg: msg }

extendErrPath :: String -> { path :: List String, msg :: String } -> { path :: List String, msg :: String }
extendErrPath p e = { path: p:e.path, msg: e.msg }

extendErr :: forall e. String -> Errors (JsonError e) -> Errors (JsonError e)
extendErr s errs =
  mapFlipped errs \err -> case prj _jsonErr err of
    Just jsonErr -> inj _jsonErr $ extendErrPath s jsonErr
    Nothing -> err

int :: forall m e. Monad m => JsonValidator m e Int
int = hoistFnV $ \v ->
  case toNumber v >>= fromNumber of
    Nothing -> failure (jsType v <> " is not an int")
    Just n -> pure n

number :: forall m e. Monad m => JsonValidator m e Number
number = hoistFnV $ \v ->
  case toNumber v of
    Nothing -> failure (jsType v <> " is not a number")
    Just x -> pure x

object :: forall m e. Monad m => Validator m (JsonError e) Json (Object Json)
object = hoistFnV $ \v ->
  case toObject v of
    Nothing -> failure (jsType v <> " is not an object")
    Just o -> pure o

string :: forall m e. Monad m => JsonValidator m e String
string = hoistFnV $ \v ->
  case toString v of
    Nothing -> failure (jsType v <> " is not a string")
    Just s -> pure s

field :: forall m e a. Monad m => String -> JsonValidator m e a -> JsonValidator m e a
field f nested = object >>> hoistFnMV (\v ->
  case lookup f v of
    Nothing -> pure $ failure ("no field " <> show f <> " in object " <> show (stringify <$> v))
    Just j -> do
      res <- runValidator nested j
      pure $ lmap (extendErr f) res)

optionalField
  :: forall m e a
   . Monad m
  => Monoid a
  => String -> JsonValidator m e a -> JsonValidator m e a
optionalField f nested = object >>> hoistFnMV (\v ->
  case lookup f v of
    Nothing -> pure $ pure mempty
    Just j -> do
      res <- runValidator nested j
      pure $ lmap (extendErr f) res)

array :: forall m e. Monad m => JsonValidator m e (Array Json)
array = hoistFnV $ \v ->
  case toArray v of
    Nothing -> failure (jsType v <> " is not an array")
    Just a -> pure a

elem :: forall m e a. Monad m => Int -> JsonValidator m e a -> JsonValidator m e a
elem i v = array >>> hoistFnMV (\arr ->
  case arr !! i of
    Nothing -> pure $ failure ("no element at index " <> show i)
    Just a -> runValidator v a)

arrayOf :: forall m e a. Monad m => JsonValidator m e a -> JsonValidator m e (Array a)
arrayOf v = array >>> hoistFnMV f
  where
    f = map sequence <<< traverse (runValidator v)
