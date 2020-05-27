module Polyform.UrlEncoded.Query where

import Prelude

import Data.FormURLEncoded (FormURLEncoded(..))
import Data.FormURLEncoded (decode) as FormURLEncoded
import Data.Map (Map)
import Data.Map (fromFoldableWith, unionWith) as Map
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
type Options = { replacePlus :: Boolean }

defaultOptions :: Options
defaultOptions = { replacePlus: true }

parse :: Options -> String -> Maybe Decoded
parse { replacePlus } query = do
  let
    query' = if replacePlus
      then replaceAll (Pattern "+") (Replacement " ") query
      else query
  FormURLEncoded decoded ← FormURLEncoded.decode query'
  pure <<< Decoded <<< Map.fromFoldableWith (<>) <<< map (map fromMaybe) $ decoded
