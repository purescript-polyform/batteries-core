module Polyform.Validators.Json
  ( JsonError
  , JsonDecodingError
  , Validator
  , array
  , arrayOf
  , boolean
  , elem
  , err
  , extendErr
  , failure
  , field
  , hoistFnMaybe
  , number
  , int
  , json
  , jsType
  , object
  , optionalField
  , string
  )
  where

import Prelude

import Data.Argonaut (Json, caseJson, jsonParser, stringify, toArray, toBoolean, toNumber, toObject, toString)
import Data.Array ((!!))
import Data.Array (singleton) as Array
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
import Polyform.Validator (hoistFnMaybe) as Polyform.Validator
import Polyform.Validators (Errors, fail)
import Polyform.Validators (Validator) as Validators

-- | Validator which builds `Json` value from `String`.
-- | It is not incorporated into default `Validator` stack
-- | because you don't have to start from `String` value
-- | (as it is in case of for example `Affjax` validator).
type JsonDecodingError e = (jsonDecoding :: String | e)
json
  :: forall e m
   . Monad m
  => Validators.Validator
      m
      (JsonDecodingError e)
      String
      Json
json = hoistFnV $ jsonParser >>> case _ of
  Right j -> pure j
  Left e -> invalid ([inj (SProxy :: SProxy "jsonDecoding") e])

type JsonError r = (json :: { path :: List String, msg :: String } | r)
type Validator m e a = Validators.Validator m (JsonError e) Json a

jsType :: Json -> String
jsType = caseJson
  (const "null")
  (const "bool")
  (const "number")
  (const "string")
  (const "array")
  (const "object")

_json = SProxy :: SProxy "json"

err :: forall e. String -> Errors (JsonError e)
err msg = Array.singleton (inj _json { path: Nil, msg: msg })

failure :: forall e a. String -> V (Errors (JsonError e)) a
failure msg = fail $ inj _json { path: Nil, msg: msg }

-- noteV :: forall e a. String -> Maybe a -> V (Errors (JsonError e)) a
-- noteV msg Nothing = failure msg
-- noteV _ (Just a) = pure a

hoistFnMaybe :: forall a e m. Applicative m => String -> (Json -> Maybe a) -> Validator m e a
hoistFnMaybe msg f = Polyform.Validator.hoistFnMaybe (err msg) f

extendErrPath
  :: String
  -> { path :: List String, msg :: String }
  -> { path :: List String, msg :: String }
extendErrPath p e = { path: p:e.path, msg: e.msg }

extendErr :: forall e. String -> Errors (JsonError e) -> Errors (JsonError e)
extendErr s errs =
  mapFlipped errs \err -> case prj _json err of
    Just jsonErr -> inj _json $ extendErrPath s jsonErr
    Nothing -> err

int :: forall m e. Monad m => Validator m e Int
int = hoistFnV $ \v ->
  case toNumber v >>= fromNumber of
    Nothing -> failure (jsType v <> " is not an int")
    Just n -> pure n

boolean :: forall m e. Monad m => Validator m e Boolean
boolean = hoistFnV $ \v ->
  case toBoolean v of
    Nothing -> failure (jsType v <> " is not a number")
    Just x -> pure x

number :: forall m e. Monad m => Validator m e Number
number = hoistFnV $ \v ->
  case toNumber v of
    Nothing -> failure (jsType v <> " is not a number")
    Just x -> pure x

object :: forall m e. Monad m => Validator m e (Object Json)
object = hoistFnV $ \v ->
  case toObject v of
    Nothing -> failure (jsType v <> " is not an object")
    Just o -> pure o

string :: forall m e. Monad m => Validator m e String
string = hoistFnV $ \v ->
  case toString v of
    Nothing -> failure (jsType v <> " is not a string")
    Just s -> pure s

field
  :: forall m e a
  . Monad m
  => String
  -> Validator m e a
  -> Validators.Validator m (JsonError e) (Object Json) a
field f nested = hoistFnMV (\v ->
  case lookup f v of
    Nothing -> pure $
      failure ("no field " <> show f <> " in object " <> show (stringify <$> v))
    Just j -> do
      res <- runValidator nested j
      pure $ lmap (extendErr f) res)

-- | Possibly less efficient as object is parsed every time
field' :: forall m e a. Monad m => String -> Validator m e a -> Validator m e a
field' f nested = object >>> field f nested

optionalField
  :: forall m e a
  . Monad m
  => String
  -> Validator m e a
  -> Validators.Validator m
      (JsonError e)
      (Object Json)
      (Maybe a)
optionalField f nested = hoistFnMV (\v ->
  case lookup f v of
    Nothing -> pure (pure Nothing)
    Just j -> do
      res <- runValidator nested j
      pure $ map Just (lmap (extendErr f) res))

optionalField'
  :: forall m e a
  . Monad m
  => String
  -> Validator m e a
  -> Validator m e (Maybe a)
optionalField' f nested = object >>> optionalField f nested

array :: forall m e. Monad m => Validator m e (Array Json)
array = hoistFnV $ \v ->
  case toArray v of
    Nothing -> failure (jsType v <> " is not an array")
    Just a -> pure a

elem :: forall m e a. Monad m => Int -> Validator m e a -> Validator m e a
elem i v = array >>> hoistFnMV (\arr ->
  case arr !! i of
    Nothing -> pure $ failure ("no element at index " <> show i)
    Just a -> runValidator v a)

arrayOf :: forall m e a. Monad m => Validator m e a -> Validator m e (Array a)
arrayOf v = array >>> hoistFnMV f
  where
    f = map sequence <<< traverse (runValidator v)

