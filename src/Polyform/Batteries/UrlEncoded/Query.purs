module Polyform.Batteries.UrlEncoded.Query where

import Prelude

import Data.FormURLEncoded (FormURLEncoded(..))
import Data.FormURLEncoded (decode) as FormURLEncoded
import Data.Map (Map)
import Data.Map (fromFoldableWith, lookup, unionWith) as Map
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), Replacement(..), replaceAll)
import Data.Unfoldable (fromMaybe)

type Key = String

type Value = Array String

-- | We need a map representation of query with
-- | appending semigroup so we can use it when
-- | composing serializers.
newtype Decoded = Decoded (Map Key Value)
derive instance newtypeDecoded ∷ Newtype Decoded _
derive newtype instance eqDecoded ∷ Eq Decoded

instance semigroupDecoded ∷ Semigroup Decoded where
  append (Decoded d1) (Decoded d2) = Decoded (Map.unionWith append d1 d2)

instance monoidDecoded ∷ Monoid Decoded where
  mempty = Decoded mempty

lookup ∷ String → Decoded → Maybe (Array String)
lookup name (Decoded q) = Map.lookup name q


-- | Browsers serialize space as `+` character
-- | which is incorrect according to the RFC 3986
-- | but it is spread behavior accross tested engines.
-- |
-- | I've written about this issue extensively:
-- | https://github.com/owickstrom/hyper/pull/62
-- |
-- | If we want to be able to optionally distinct this `+`
-- | on the server side we have to convert it to `%2b` before
-- | decoding phase (as it is done in all investigated
-- | libraries - please check first post in the above thread).
type Options = { replacePlus ∷ Boolean }

defaultOptions ∷ Options
defaultOptions = { replacePlus: true }

parse ∷ Options → String → Maybe Decoded
parse { replacePlus } query = do
  let
    query' = if replacePlus
      then replaceAll (Pattern "+") (Replacement " ") query
      else query
  FormURLEncoded decoded ← FormURLEncoded.decode query'
  pure <<< Decoded <<< Map.fromFoldableWith (<>) <<< map (map fromMaybe) $ decoded


-- | Parser
-- type Error fieldErrs errs =
--   ( urlEncodedQueryParseError ∷ String
--   , fieldError ∷ FieldError fieldErrs
--   | errs
--   )

-- _queryParseError = SProxy ∷ SProxy "queryParseError"
-- 
-- query ∷ ∀ m e errs. Monad m ⇒ Query.Options → Batteries.Validator m (Error e errs) String Query.Decoded
-- query opts = Validator.liftFnMaybe (Batteries.error _queryParseError) (Query.parse opts)
-- 
-- _fieldError = SProxy ∷ SProxy "fieldError"
-- | Dual
-- query ∷ ∀ errs fieldErrs m s
--   . Monad m
--   ⇒ Applicative s
--   ⇒ Query.Options
--   → Batteries.Dual m s (Error fieldErrs errs) String Query.Decoded
-- query opts = dual (Validators.query opts) serializer
--   where
--     serializer (Query.Decoded m) =
--       pure <<< unsafeEncode <<< FormURLEncoded <<< Array.Builder.build <<< foldMapWithIndex step $ m
--       where
--         step key [] = Array.Builder.cons (Tuple key Nothing)
--         step key values = foldMap (\v → Array.Builder.cons (Tuple key (Just v))) values
-- 
--     -- | TODO: Make this safe by dropping unsupported codeunits:
--     -- |
--     -- | * https://stackoverflow.com/questions/16868415/encodeuricomponent-throws-an-exception
--     -- |
--     -- | * urlPart = urlPart.replace(/[\ud800-\udfff]/g, '');
-- 
--     unsafeEncode ∷ FormURLEncoded → String
--     unsafeEncode = String.joinWith "&" <<< map encodePart <<< FormURLEncoded.toArray
--       where
--         encodePart = case _ of
--           Tuple k Nothing -> unsafeEncodeURIComponent k
--           Tuple k (Just v) -> unsafeEncodeURIComponent k <> "=" <> unsafeEncodeURIComponent v

