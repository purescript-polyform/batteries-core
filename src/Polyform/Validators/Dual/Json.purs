module Polyform.Validators.Dual.Json where

import Prelude

import Data.Argonaut (Json, fromNumber, fromObject, fromString) as Argonaut
import Data.Argonaut (stringify)
import Data.Bifunctor (lmap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Semigroup.First (First(..))
import Foreign.Object (Object, lookup)
import Foreign.Object (singleton) as Object
import Polyform.Validator (hoistFn, hoistFnMV, runValidator)
import Polyform.Validator.Dual (Dual(..), DualD(..)) as Validator
import Polyform.Validators (Errors) as Validtors
import Polyform.Validators.Json (JsError, extendErr, failure)
import Polyform.Validators.Json (int, number, object, string) as Validators.Json

type Dual m e a b = Validator.Dual m (Validtors.Errors e) a b
type JsonDual m e a = Dual m (JsError e) Argonaut.Json a

object :: forall e m. Monad m => JsonDual m e (Object Argonaut.Json)
object = Validator.Dual $ Validator.DualD $
  { validator: Validators.Json.object
  , serializer: Argonaut.fromObject
  }

int :: forall m e. Monad m => JsonDual m e Int
int = Validator.Dual $ Validator.DualD $
  { validator: Validators.Json.int
  , serializer: Argonaut.fromNumber <<< toNumber
  }

number :: forall m e. Monad m => JsonDual m e Number
number = Validator.Dual $ Validator.DualD $
  { validator: Validators.Json.number
  , serializer: Argonaut.fromNumber
  }

string :: forall m e. Monad m => JsonDual m e String
string = Validator.Dual $ Validator.DualD $
  { validator: Validators.Json.string
  , serializer: Argonaut.fromString
  }

type ObjectDual m e a = Dual m (JsError e) (Object (First Argonaut.Json)) a

objectField :: forall m e a. Monad m => String -> JsonDual m e a -> ObjectDual m e a
objectField label (Validator.Dual (Validator.DualD nested)) =
  Validator.Dual $ Validator.DualD $ { serializer, validator }
  where
    validator = hoistFnMV \obj ->
      case lookup label obj of
        Nothing -> pure $ failure ("no field " <> show label <> " in object " <> show ((stringify <<< unwrap) <$> obj))
        Just (First json) -> do
          res <- runValidator nested.validator json
          pure $ lmap (extendErr label) res
    serializer = nested.serializer >>> First >>> Object.singleton label

newtypeD ∷ ∀ a e m n. Monad m ⇒ Newtype n a ⇒ Dual m e a n
newtypeD = Validator.Dual $ Validator.DualD $
  { serializer: unwrap
  , validator: hoistFn wrap
  }

mapD ∷ ∀ a b e t m. Monad m ⇒ Functor t ⇒ (a → b) → (b → a) → Dual m e (t a) (t b)
mapD f g = Validator.Dual $ Validator.DualD $
  { serializer: map g
  , validator: hoistFn (map f)
  }
