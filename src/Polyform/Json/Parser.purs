module Polyform.Json.Parser where

import Prelude

import Data.Argonaut (Json)
import Data.Argonaut (jsonParser, stringify) as Argonaut
import Data.Either (Either(..))
import Polyform.Dual (dual) as Dual
import Polyform.Validator (liftFnV) as Validator
import Polyform.Validators (Dual, Validator, invalid) as Validators
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

_decodingError = SProxy ∷ SProxy "jsonDecodingError"

type JsonDecodingError e = (jsonDecodingError ∷ String | e)

validator
  ∷ ∀ errs m
  . Monad m
  ⇒ Validators.Validator
      m
      (JsonDecodingError + errs)
      String
      Json
validator = Validator.liftFnV $ Argonaut.jsonParser >>> case _ of
  Right j → pure j
  Left e → Validators.invalid _decodingError e

dual
  ∷ ∀ e m s
  . Monad m
  ⇒ Applicative s
  ⇒ Validators.Dual m s (JsonDecodingError + e) String Json
dual = Dual.dual
  validator
  (pure <<< Argonaut.stringify)

