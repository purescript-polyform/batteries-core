module Polyform.Validators.Json
  ( JsError
  , JsValidation
  , array
  , arrayOf
  , elem
  , failure
  , field
  , number
  , int
  , object
  , optionalField
  , string
  ) where

import Prelude

import Data.Argonaut (Json, caseJson, toArray, toNumber, toObject, toString, stringify)
import Data.Array ((!!))
import Data.Bifunctor (lmap)
import Data.Int (fromNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, traverse)
import Data.Variant (inj, prj)
import Foreign.Object (Object, lookup)
import Polyform.Validation (V, hoistFnMV, hoistFnV, runValidation)
import Polyform.Validators (Errors, Validator, fail)

type JsError r = (jsError :: { path :: List String, msg :: String } | r)
type JsValidation m e a = Validator m (JsError e) Json a

jsType :: Json -> String
jsType = caseJson
  (const "null")
  (const "bool")
  (const "number")
  (const "string")
  (const "array")
  (const "object")

_jsErr = SProxy :: SProxy "jsError"

failure :: forall e a. String -> V (Errors (JsError e)) a
failure msg = fail $ inj _jsErr { path: Nil, msg: msg }

extendPath :: String -> { path :: List String, msg :: String } -> { path :: List String, msg :: String }
extendPath p e = { path: p:e.path, msg: e.msg } 

extend :: forall e. String -> Errors (JsError e) -> Errors (JsError e)
extend s errs =
  map (\err -> case prj _jsErr err of
    Just jsErr -> inj _jsErr $ extendPath s jsErr
    Nothing -> err) errs

int :: forall m e. Monad m => JsValidation m e Int
int = hoistFnV $ \v ->
  case toNumber v >>= fromNumber of
    Nothing -> failure (jsType v <> " is not an int")
    Just n -> pure n

number :: forall m e. Monad m => JsValidation m e Number
number = hoistFnV $ \v ->
  case toNumber v of
    Nothing -> failure (jsType v <> " is not a number")
    Just x -> pure x

object :: forall m e. Monad m => Validator m (JsError e) Json (Object Json)
object = hoistFnV $ \v ->
  case toObject v of
    Nothing -> failure (jsType v <> " is not an object")
    Just o -> pure o

string :: forall m e. Monad m => JsValidation m e String
string = hoistFnV $ \v ->
  case toString v of
    Nothing -> failure (jsType v <> " is not a string")
    Just s -> pure s

field :: forall m e a. Monad m => String -> JsValidation m e a -> JsValidation m e a
field f nested = object >>> hoistFnMV (\v ->
  case lookup f v of
    Nothing -> pure $ failure ("no field " <> show f <> " in object " <> show (stringify <$> v))
    Just json -> do
      res <- runValidation nested json
      pure $ lmap (extend f) res)

optionalField
  :: forall m e a
   . Monad m
  => Monoid a
  => String -> JsValidation m e a -> JsValidation m e a
optionalField f nested = object >>> hoistFnMV (\v ->
  case lookup f v of
    Nothing -> pure $ pure mempty
    Just json -> do
      res <- runValidation nested json
      pure $ lmap (extend f) res)

array :: forall m e. Monad m => JsValidation m e (Array Json)
array = hoistFnV $ \v ->
  case toArray v of
    Nothing -> failure (jsType v <> " is not an array")
    Just a -> pure a

elem :: forall m e a. Monad m => Int -> JsValidation m e a -> JsValidation m e a
elem i v = array >>> hoistFnMV (\arr ->
  case arr !! i of
    Nothing -> pure $ failure ("no element at index " <> show i)
    Just a -> runValidation v a)

arrayOf :: forall m e a. Monad m => JsValidation m e a -> JsValidation m e (Array a)
arrayOf v = array >>> hoistFnMV f
  where
    f = map sequence <<< traverse (runValidation v)
