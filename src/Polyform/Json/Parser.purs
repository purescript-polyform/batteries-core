module Polyform.Json.Parser where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut (jsonParser) as Argonaut
import Data.Either (Either(..))
import Polyform.Validator (liftFnV) as Validator
import Polyform.Validators (Errors, Validator, invalid) as Validators
import Type.Prelude (SProxy(..))

type ErrorsRow errs = Validators.Errors (jsonDecodingError ∷ String | errs)

_decodingError = SProxy ∷ SProxy "jsonDecodingError"

validator
  ∷ ∀ errs m
  . Monad m
  ⇒ Validators.Validator
      m
      (jsonDecodingError ∷ String | errs)
      String
      Json
validator = Validator.liftFnV $ Argonaut.jsonParser >>> case _ of
  Right j → pure j
  Left e → Validators.invalid _decodingError e


