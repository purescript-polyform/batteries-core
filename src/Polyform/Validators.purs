module Polyform.Validators
  ( Validator(..)
  , Errors(..)
  , fail
  ) where

import Prelude

import Data.Array (singleton)
import Data.Variant (Variant)
import Polyform.Validation (Validation, V(..))

type Errors e = Array (Variant e)
type Validator m e a = Validation m (Errors e) a

fail :: forall e a. Variant e -> V (Errors e) a
fail e = Invalid $ singleton e
