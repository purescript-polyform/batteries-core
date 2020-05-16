module Polyform.Validators
  ( Validator(..)
  , Errors(..)
  , check
  , fail
  ) where

import Prelude

import Data.Array (singleton)
import Data.Variant (Variant)
import Data.Validation.Semigroup (invalid, unV, V)
import Polyform.Validator (Validator) as Polyform
import Polyform.Validator (hoistFnMV, runValidator)

type Errors e = Array (Variant e)

type Validator m e a = Polyform.Validator m (Errors e) a

fail ∷ ∀ e a. Variant e → V (Errors e) a
fail e = invalid $ singleton e

check
  ∷ ∀ e m a
  . Monoid e
  ⇒ Monad m
  ⇒ (a → e)
  → Polyform.Validator m e a Boolean
  → Polyform.Validator m e a a
check msg pred = hoistFnMV $ \v → do
  b ← runValidator pred v
  pure $ unV
    invalid
    (if _ then pure v else invalid (msg v))
    b
