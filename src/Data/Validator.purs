module Data.Validator
  ( Validator(..)
  , Errors(..)
  , fail

  -- Export until updated to 0.12
  , RowApply(..)
  , type (+)
  ) where

import Prelude

import Data.Array (singleton)
import Data.Variant(Variant)
import Polyform.Validation(Validation, V(..))

type RowApply (f :: # Type -> # Type) (a :: # Type) = f a
infixr 0 type RowApply as +

type Errors e = Array (Variant e)
type Validator m e a = Validation m (Errors e) a

fail :: forall e a. Variant e -> V (Errors e) a
fail e = Invalid $ singleton e
