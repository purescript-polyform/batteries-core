module Validators.UrlEncoded 
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

import Data.Array (fromFoldable)
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.String (toLower)
import Data.Symbol (SProxy(..))
import Data.URI (Query(..))
import Data.URI.Query (parser)
import Data.Validator (Errors, Validator, fail)
import Data.Variant (inj)
import Polyform.Validation (V(..), hoistFnV)
import Text.Parsing.StringParser (ParseError(..), runParser)

type UrlValidation m e a b = Validator m (UrlError e) a b
type UrlError e = (urlError :: ParseError | e)
type UrlEncoded = StrMap (Array String)

_urlErr :: SProxy "urlError"
_urlErr = SProxy

failure :: forall e a. String -> V (Errors (UrlError e)) a
failure s = fail $ inj _urlErr $ ParseError s

fromEither :: forall e a. Either ParseError a -> V (Errors (UrlError e)) a
fromEither (Left e) = fail $ inj _urlErr e
fromEither (Right v) = Valid [] v

urlEncoded :: forall m e. Monad m => UrlValidation m e String UrlEncoded
urlEncoded = hoistFnV \s ->
  (queryToMap <$> (fromEither $ runParser parser ("?" <> s)))

queryToMap :: Query -> UrlEncoded
queryToMap (Query q) =
  let q' = map (rmap fromFoldable) q
  in StrMap.fromFoldableWith (<>) q'


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
single f = hoistFnV $ \q -> case StrMap.lookup f q of
  Just [s] -> pure s
  _ -> failure $ "Could not find field " <> f

array :: forall m e. Monad m => String -> UrlValidation m e UrlEncoded (Array String)
array f = hoistFnV $ \q -> case StrMap.lookup f q of
  Just s -> pure s
  Nothing -> failure $ "Could not find field " <> f
