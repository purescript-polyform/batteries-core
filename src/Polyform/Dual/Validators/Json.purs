module Polyform.Dual.Validators.Json
  ( Dual
  , JsonDual
  , ObjectDual
  , Object
  , int
  , json
  , number
  , mapDual
  , newtypeDual
  , object
  , objectField
  , objectField'
  , string
  , (:=)
  )
  where

import Prelude

import Data.Argonaut (Json, fromNumber, fromObject, fromString, stringify) as Argonaut
import Data.Argonaut (Json, stringify)
import Data.Bifunctor (lmap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Semigroup.First (First(..))
import Foreign.Object (Object, lookup, singleton) as Foreign
import Polyform.Dual (DualD, dual, (>-))
import Polyform.Dual.Validator as Dual.Validator
import Polyform.Validator (Validator, hoistFn, hoistFnMV, runValidator)
import Polyform.Validators (Errors) as Validators
import Polyform.Validators.Json (JsonError, JsonDecodingError, extendErr, failure)
import Polyform.Validators.Json (int, json, number, object, string) as Validators.Json
import Prim.Row (class Cons)
import Record (get)
import Type.Data.Symbol (SProxy)
import Type.Prelude (class IsSymbol, reflectSymbol)

json :: forall e m
   . Monad m
  => Dual m (JsonDecodingError e) String Json
json = dual
  { parser: Validators.Json.json
  , serializer: Argonaut.stringify
  }

type Dual m e a b = Dual.Validator.Validator m (Validators.Errors e) a b
type JsonDual m e a = Dual m (JsonError e) Argonaut.Json a

type Object a = Foreign.Object (First a)

object :: forall e m. Monad m => JsonDual m e (Object Argonaut.Json)
object = dual
  { parser: map First <$> Validators.Json.object
  , serializer: Argonaut.fromObject <<< map runFirst
  }
  where
    runFirst (First a) = a

int :: forall m e. Monad m => JsonDual m e Int
int = dual
  { parser: Validators.Json.int
  , serializer: Argonaut.fromNumber <<< toNumber
  }

number :: forall m e. Monad m => JsonDual m e Number
number = dual
  { parser: Validators.Json.number
  , serializer: Argonaut.fromNumber
  }

string :: forall m e. Monad m => JsonDual m e String
string = dual
  { parser: Validators.Json.string
  , serializer: Argonaut.fromString
  }

type ObjectDual m e a = Dual m (JsonError e) (Object Argonaut.Json) a

objectField :: forall m e a. Monad m => String -> JsonDual m e a -> ObjectDual m e a
objectField label d =
  dual $ { serializer, parser }
  where
    fieldDual = unwrap $ unwrap d
    parser = hoistFnMV \obj ->
      case Foreign.lookup label obj of
        Nothing -> pure $ failure ("no field " <> show label <> " in object " <> show ((stringify <<< unwrap) <$> obj))
        Just (First j) -> do
          res <- runValidator fieldDual.parser j
          pure $ lmap (extendErr label) res
    serializer = fieldDual.serializer >>> First >>> Foreign.singleton label

objectField' :: forall a e m r r' s
  . IsSymbol s
  => Cons s a r' r
  => Monad m
  => SProxy s
  -> JsonDual m e a
  -> DualD (Validator m (Validators.Errors (JsonError e))) (Object Json) { | r } a
objectField' label validator =
  get label >- objectField (reflectSymbol label) validator

infix 7 objectField' as :=

newtypeDual :: forall a e m n. Monad m => Newtype n a => Dual m e a n
newtypeDual = dual
  { parser: hoistFn wrap
  , serializer: unwrap
  }

mapDual :: forall a b e m t. Monad m => Functor t => (a -> b) -> (b -> a) -> Dual m e (t a) (t b)
mapDual f g = dual
  { parser: hoistFn (map f)
  , serializer: map g
  }
