module Polyform.UrlEncoded.Parser where

import Prelude

import Control.Monad.Except (throwError)
import Data.Array (cons, sortWith)
import Data.Array (filter) as Array
import Data.Either (Either)
import Data.Foldable (foldr)
import Data.Map (fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, split)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), fst)
import Polyform.UrlEncoded.Types (Decoded)

foreign import decodeURIComponentImpl :: String -> Nullable String

decodeURIComponent :: String -> Maybe String
decodeURIComponent = toMaybe <<< decodeURIComponentImpl

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

parse :: Options -> String -> Either String Decoded
parse opts
  = split (Pattern "&")
  >>> Array.filter (_ /= "")
  >>> map (split (Pattern "="))
  >>> map toTuple
  >>> sequence
  >>> map (sortWith fst)
  -- | XXX: mapping and folding was somewhat cumbersome
  >>> map folded
  >>> map fromFoldable
  where
    folded :: Array (Tuple String (Array String)) -> Array (Tuple String (Array String))
    folded arr = build $ foldr step Nothing arr
      where
        build (Just { curr, result }) = cons (Tuple curr.key curr.accum) result
        build Nothing = []
        step (Tuple key vs) Nothing =
          Just { curr: { key, accum: vs }, result: [] }
        step (Tuple key vs) (Just { curr, result }) = Just $
          if curr.key == key
            then
              { curr: curr { accum = vs <> curr.accum }, result }
            else
              { curr: { key, accum: vs }, result: cons (Tuple curr.key curr.accum) result }

    toTuple :: Array String -> Either String (Tuple String (Array String))
    toTuple kv =
      case kv of
        [key] -> throwError ("Missing value for key: \"" <> key <> "\"")
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
              Just key', Just value'' -> pure (Tuple key' [value''])
              Nothing, Just _ -> throwError (keyDecodingError key)
              Just key', Nothing -> throwError (valueDecodingError key' value)
              Nothing, Nothing -> throwError (keyDecodingError key <> ", " <> valueDecodingError key value)
        parts ->
          throwError ("Invalid key-value pair: " <> joinWith " " parts)
      where
        keyDecodingError key = "Unable to decode key: " <> key
        valueDecodingError key value
          = "Unable to decode key value: key = "
          <> show key
          <> ", value = "
          <> show value

