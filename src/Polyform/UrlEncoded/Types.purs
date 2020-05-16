module Polyform.UrlEncoded.Types where

import Data.Map (Map)
import Data.Maybe (Maybe)
import Polyform.Validators (Validator) as Validator
import Type.Prelude (SProxy(..))

type Error e =
  ( urlDecoding :: String
  , urlValueParsing ::
      { error :: Array String
      , field :: String
      , input :: Maybe (Array String)
      }
  | e
  )
type Validator m e a b = Validator.Validator m (Error e) a b

type Key = String
type Value = Array String
type Decoded = Map Key Value

_urlDecoding :: SProxy "urlDecoding"
_urlDecoding = SProxy

_urlValueParsing :: SProxy "urlValueParsing"
_urlValueParsing = SProxy
