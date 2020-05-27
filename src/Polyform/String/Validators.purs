module Polyform.String.Validators where

import Prelude

import Polyform.Validator (check) as Validator
import Polyform.Validators (Validator, error) as Validators
import Type.Prelude (SProxy(..))

_nonBlankExpected = SProxy ∷ SProxy "nonBlankExpected"

nonBlank ∷ ∀ e m. Monad m ⇒ Validators.Validator m (nonBlankExpected ∷ Unit | e) String String
nonBlank = Validator.check
  (const $ Validators.error _nonBlankExpected unit)
  (_ /= "")

