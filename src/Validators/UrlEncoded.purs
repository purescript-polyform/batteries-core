module Validators.UrlEncoded 
  ( UrlValidation
  , array
  , boolean
  , int
  , number
  , single
  , urlEncoded
  ) where

import Prelude

import Data.Array (fromFoldable, singleton)
import Data.Bifunctor (lmap, rmap)
import Data.Either (Either)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.String (toLower)
import Data.URI (Query(..))
import Data.URI.Query (parser)
import Polyform.Validation (V(..), Validation, hoistFnV)
import Polyform.Validation as V
import Text.Parsing.StringParser (ParseError(..), runParser)

type UrlValidation m a b = Validation m (Array ParseError) a b
type UrlEncoded = StrMap (Array String)

fail :: forall a. String -> V (Array ParseError) a
fail s = Invalid $ singleton $ ParseError s

fromEither :: forall a. Either ParseError a -> V (Array ParseError) a
fromEither = lmap singleton >>> V.fromEither

urlEncoded :: forall m. Monad m => UrlValidation m String UrlEncoded
urlEncoded = hoistFnV \s ->
  (queryToMap <$> (fromEither $ runParser parser ("?" <> s)))

queryToMap :: Query -> UrlEncoded
queryToMap (Query q) =
  let q' = map (rmap fromFoldable) q
  in StrMap.fromFoldableWith (<>) q'


number :: forall m. Monad m => UrlValidation m String Number
number = hoistFnV $ \s -> case Number.fromString s of
  Just n -> pure n
  Nothing -> fail $ "Could not parse " <> s <> " as number"

int :: forall m. Monad m => UrlValidation m String Int
int = hoistFnV $ \s -> case Int.fromString s of
  Just n -> pure n
  Nothing -> fail $ "Could not parse " <> s <> " as int"

boolean :: forall m. Monad m => UrlValidation m String Boolean
boolean = hoistFnV $ \s -> case toLower s of
  "false" -> pure false
  "true" -> pure true
  _ -> fail $ "Could not parse " <> s <> " as boolean"

single :: forall m. Monad m => String -> UrlValidation m UrlEncoded String
single f = hoistFnV $ \q -> case StrMap.lookup f q of
  Just [s] -> pure s
  _ -> fail $ "Could not find field " <> f

array :: forall m. Monad m => String -> UrlValidation m UrlEncoded (Array String)
array f = hoistFnV $ \q -> case StrMap.lookup f q of
  Just s -> pure s
  Nothing -> fail $ "Could not find field " <> f
