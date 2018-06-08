module Validators.Json 
  ( JsError
  , JsErr
  , JsValidation
  , Err
  , array
  , arrayOf
  , elem
  , fail
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
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence, traverse)
import Data.Variant(Variant, inj, prj)
import Polyform.Validation (V(..), Validation, hoistFnMV, hoistFnV, runValidation)

type RowApply (f :: # Type -> # Type) (a :: # Type) = f a

infixr 0 type RowApply as +


type JsErr r = (jsError :: JsError | r)

newtype JsError = JsErr { path :: List String, msg :: String }

instance showJsError :: Show JsError where
  show (JsErr e) = "(Error at" <> show (fromFoldable e.path) <> " " <> e.msg <> ")"

type Err e = Array (Variant (JsErr e))
type JsValidation m e a = Validation m (Err e) Json a

jsType :: Json -> String
jsType = foldJson
  (const "null")
  (const "bool")
  (const "number")
  (const "string")
  (const "array")
  (const "object")

_jsErr = SProxy :: SProxy "jsError"

fail :: forall e a. String -> V (Err e) a
fail msg = Invalid $ singleton $ inj _jsErr $ JsErr { path: Nil, msg: msg }

extendPath :: String -> JsError -> JsError
extendPath p (JsErr e) = JsErr { path: p:e.path, msg: e.msg } 

extend :: forall e. String -> Err e -> Err e
extend s e =
  map (\e -> case prj _jsErr e of
    Just e -> inj _jsErr $ extendPath s e
    Nothing -> e) e

int :: forall m e. Monad m => JsValidation m e Int
int = hoistFnV $ \v ->
  case toNumber v >>= fromNumber of
    Nothing -> fail (jsType v <> " is not an int")
    Just n -> pure n

object :: forall m e. Monad m => Validation m (Err e) Json (StrMap Json)
object = hoistFnV $ \v ->
  case toObject v of
    Nothing -> fail (jsType v <> " is not an object")
    Just o -> pure o

string :: forall m e. Monad m => JsValidation m e String
string = hoistFnV $ \v ->
  case toString v of
    Nothing -> fail (jsType v <> " is not a string")
    Just s -> pure s

field :: forall m e a. Monad m => String -> JsValidation m e a -> JsValidation m e a
field f nested = object >>> hoistFnMV (\v ->
  case lookup f v of
    Nothing -> pure $ fail ("no field " <> show f <> " in object " <> show v)
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
    Nothing -> fail (jsType v <> " is not an array")
    Just a -> pure a

elem :: forall m e a. Monad m => Int -> JsValidation m e a -> JsValidation m e a
elem i v = array >>> hoistFnMV (\arr ->
  case arr !! i of
    Nothing -> pure $ fail ("no element at index " <> show i)
    Just a -> runValidation v a)

arrayOf :: forall m e a. Monad m => JsValidation m e a -> JsValidation m e (Array a)
arrayOf v = array >>> hoistFnMV f
  where
    f = map sequence <<< traverse (runValidation v)
