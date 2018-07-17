module Polyform.Validators
  ( Validator(..)
  , Errors(..)
  , fail
  ) where

import Prelude

import Data.Array (singleton)
import Data.Variant (Variant)
import Polyform.Validation (V(..), Validation, hoistFnMV, runValidation)

type Errors e = Array (Variant e)
type Validator m e a = Validation m (Errors e) a

fail :: forall e a. Variant e -> V (Errors e) a
fail e = Invalid $ singleton e

check
  :: forall e m a
   . Monoid e
  => Monad m => (a -> e) -> Validation m e a Boolean -> Validation m e a a
check msg pred = hoistFnMV $ \v -> do
  b <- runValidation pred v
  pure $ case b of
    Valid e true -> Valid e v
    Valid e false -> Invalid $ (msg v) <> e
    Invalid e -> Invalid e
