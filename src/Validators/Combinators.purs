module Validators.Combinators where

import Prelude

import Polyform.Validation (V(..), Validation, hoistFnMV, runValidation)

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
