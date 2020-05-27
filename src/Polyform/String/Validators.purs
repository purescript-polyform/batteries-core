module Polyform.String.Validators where

import Prelude

import Polyform.Validator (check) as Validator
import Polyform.Validators (Validator, error) as Validators
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

_nonBlankExpected = SProxy ∷ SProxy "nonBlankExpected"

type NonBlankExpected e = (nonBlankExpected ∷ Unit | e)

nonBlank ∷ ∀ e m. Monad m ⇒ Validators.Validator m (NonBlankExpected + e) String String
nonBlank = Validator.check
  (const $ Validators.error _nonBlankExpected unit)
  (_ /= "")

