module Polyform.Validators.UrlEncoded
  ( UrlValidation
  , UrlError
  , array
  , boolean
  , int
  , number
  , single
  , urlEncoded
  ) where

import Prelude

import Control.Monad.Except (throwError)
import Data.Array (filter) as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.Number as Number
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, split, toLower)
import Data.Symbol (SProxy(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Variant (inj)
import Foreign.Object (Object, fromFoldable, lookup)
import Polyform.Validation (V(..), hoistFnV)
import Polyform.Validators (Errors, Validator, fail)

type UrlValidation m e a b = Validator m (UrlError e) a b
type UrlError e = (urlError :: String | e)
type UrlEncoded = Object (Array String)

_urlErr :: SProxy "urlError"
_urlErr = SProxy

failure :: forall e a. String -> V (Errors (UrlError e)) a
failure s = fail $ inj _urlErr $ s

fromEither :: forall e a. Either String a -> V (Errors (UrlError e)) a
fromEither (Left e) = fail $ inj _urlErr e
fromEither (Right v) = Valid [] v

foreign import decodeURIComponentImpl :: String -> Nullable String

decodeURIComponent :: String -> Maybe String
decodeURIComponent = toMaybe <<< decodeURIComponentImpl

-- | I've written about this issue extensively:
-- | https://github.com/owickstrom/hyper/pull/62
-- |
-- | Shortly: browsers serialize space as `+` character
-- | which is incorrect according to the RFC 3986
-- | but it is spread behavior accross tested engines.
-- |
-- | If we want to be able to optionally distinct this `+`
-- | on the server side we have to convert it to `%2b` before
-- | decoding phase (as it is done in all investigated
-- | libraries - please check first post in the above thread).
type Options = { replacePlus :: Boolean }

defaultOptions :: Options
defaultOptions = { replacePlus: true }

parse :: Options -> String -> Either String UrlEncoded
parse opts
  = split (Pattern "&")
  >>> Array.filter (_ /= "")
  >>> map (split (Pattern "="))
  >>> map toTuple
  >>> sequence
  >>> map fromFoldable
  where
  toTuple :: Array String -> Either String (Tuple String (Array String))
  toTuple kv =
    case kv of
      [key] -> case decodeURIComponent key of
        Nothing → throwError (keyDecodingError key)
        Just key' → pure (Tuple key' [])
      [key, value] ->
        let
          value' =
            if opts.replacePlus
              then
                replaceAll (Pattern "+") (Replacement " ") value
              else
                value
        in
          -- XXX we should probably change UrlError so it aggregates list of errors
          case (decodeURIComponent key), (decodeURIComponent value') of
            Just key', Just value'' → pure (Tuple key [value''])
            Nothing, Just _ → throwError (keyDecodingError key)
            Just key', Nothing → throwError (valueDecodingError key' value)
            Nothing, Nothing → throwError (keyDecodingError key <> ", " <> valueDecodingError key value)
      parts ->
        throwError ("Invalid form key-value pair: " <> joinWith " " parts)
    where
    keyDecodingError key = "Unable to decode key: " <> key
    valueDecodingError key value
      = "Unable to decode key value: key = "
      <> show key
      <> ", value = "
      <> show value

urlEncoded :: forall m e. Monad m => Options → UrlValidation m e String UrlEncoded
urlEncoded opts = hoistFnV $ \s -> fromEither (parse opts s)

number :: forall m e. Monad m => UrlValidation m e String Number
number = hoistFnV $ \s -> case Number.fromString s of
  Just n -> pure n
  Nothing -> failure $ "Could not parse " <> s <> " as number"

int :: forall m e. Monad m => UrlValidation m e String Int
int = hoistFnV $ \s -> case Int.fromString s of
  Just n -> pure n
  Nothing -> failure $ "Could not parse " <> s <> " as int"

boolean :: forall m e. Monad m => UrlValidation m e String Boolean
boolean = hoistFnV $ \s -> case toLower s of
  "false" -> pure false
  "true" -> pure true
  _ -> failure $ "Could not parse " <> s <> " as boolean"

single :: forall m e. Monad m => String -> UrlValidation m e UrlEncoded String
single f = hoistFnV $ \q -> case lookup f q of
  Just [s] -> pure s
  _ -> failure $ "Could not find field " <> f

array :: forall m e. Monad m => String -> UrlValidation m e UrlEncoded (Array String)
array f = hoistFnV $ \q -> case lookup f q of
  Just s -> pure s
  Nothing -> failure $ "Could not find field " <> f
