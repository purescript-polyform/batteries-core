module Polyform.Validators.UrlEncoded.Types where

import Data.Map (Map)
import Polyform.Validators (Validator) as Validator
import Type.Prelude (SProxy(..))

type Error e = (urlDecoding :: String, urlField :: { field :: String, error :: String } | e)
type Validator m e a b = Validator.Validator m (Error e) a b

type Key = String
type Value = Array String
type Decoded = Map Key Value

_urlDecoding :: SProxy "urlDecoding"
_urlDecoding = SProxy

_urlField :: SProxy "urlField"
_urlField = SProxy
