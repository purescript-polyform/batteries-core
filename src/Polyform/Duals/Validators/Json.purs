module Polyform.Duals.Validators.Json
  ( Dual
  , JsonDual
  , ObjectDual
  , Object
  , boolean
  , int
  , insert
  , json
  , number
  , mapDual
  , newtypeDual
  , object
  , field
  , fieldProp
  , string
  , (:=)
  , variant
  )
  where

import Prelude

import Data.Argonaut (Json, fromBoolean, fromNumber, fromObject, fromString, stringify) as Argonaut
import Data.Argonaut (Json, stringify)
import Data.Bifunctor (lmap)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Semigroup.First (First(..))
import Data.Variant (Variant)
import Foreign.Object (Object, lookup, singleton) as Foreign
import Polyform.Dual (Dual(..), DualD(..)) as Dual
import Polyform.Dual (DualD(..), dual, (~))
import Polyform.Dual.Generic (class GDualVariant)
import Polyform.Dual.Record (RecordBuilder, insert) as Dual.Record
import Polyform.Duals.Validator as Duals.Validator
import Polyform.Duals.Validator.Generic (variant) as Duals.Validator.Generic
import Polyform.Validator (Validator) as Polyform
import Polyform.Validator (Validator, hoistFn, hoistFnMV, hoistFnV, runValidator, valid)
import Polyform.Validators (Errors) as Validators
import Polyform.Validators.Json (JsonDecodingError, JsonError, extendErr, failure)
import Polyform.Validators.Json (boolean, int, json, number, object, string) as Validators.Json
import Prim.Row (class Cons)
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList)
import Record (get)
import Type.Data.Symbol (SProxy)
import Type.Prelude (class IsSymbol, reflectSymbol)

json :: forall e m
   . Monad m
  => Dual m (JsonDecodingError e) String Json
json = dual
  Validators.Json.json
  Argonaut.stringify

type Dual m e a b = Duals.Validator.Dual m (Validators.Errors e) a b
type JsonDual m e a = Dual m (JsonError e) Argonaut.Json a

type Object a = Foreign.Object (First a)

object :: forall e m. Monad m => JsonDual m e (Object Argonaut.Json)
object = dual
  (map First <$> Validators.Json.object)
  (Argonaut.fromObject <<< map runFirst)
  where
    runFirst (First a) = a

int :: forall m e. Monad m ⇒ JsonDual m e Int
int = dual
  Validators.Json.int
  (Argonaut.fromNumber <<< toNumber)

boolean ∷ ∀ m e. Monad m ⇒ JsonDual m e Boolean
boolean = dual
  Validators.Json.boolean
  Argonaut.fromBoolean

number :: forall m e. Monad m => JsonDual m e Number
number = dual
  Validators.Json.number
  Argonaut.fromNumber

string :: forall m e. Monad m => JsonDual m e String
string = dual
  Validators.Json.string
  Argonaut.fromString

type ObjectDual m e a = Dual m (JsonError e) (Object Argonaut.Json) a

field :: forall m e a. Monad m => String -> JsonDual m e a -> ObjectDual m e a
field label d =
  dual prs ser
  where
    DualD fieldPrs fieldSer = unwrap d
    prs = hoistFnMV \obj ->
      case Foreign.lookup label obj of
        Nothing -> pure $ failure ("no field " <> show label <> " in object " <> show ((stringify <<< unwrap) <$> obj))
        Just (First j) -> do
          res <- runValidator fieldPrs j
          pure $ lmap (extendErr label) res
    ser = fieldSer >>> First >>> Foreign.singleton label

-- | Less efficient version which parses / serializes Json
-- | object to extract field value.
field' :: forall m e a. Monad m => String -> JsonDual m e a -> JsonDual m e a
field' label d = object >>> field label d


-- insert :: forall a e m r r' s
--   . IsSymbol s
--   => Cons s a r' r
--   => Monad m
--   => SProxy s
--   -> JsonDual m e a
--   -> DualD (Validator m (Validators.Errors (JsonError e))) (Object Json) { | r } a

--type Dual m e a b = Duals.Validator.Dual m (Validators.Errors e) a b

insert ∷ ∀ e m l o prs prs' ser ser'
  . Row.Cons l o ser ser'
  ⇒ Row.Lacks l ser
  ⇒ Row.Cons l o prs prs'
  ⇒ Row.Lacks l prs
  ⇒ IsSymbol l
  ⇒ Monad m
  ⇒ SProxy l
  → JsonDual m e o
  → Dual.Record.RecordBuilder
    (Polyform.Validator m (Validators.Errors (JsonError e)))
    (Object Json)
    { | ser'}
    { | prs}
    { | prs'}
insert label dual =
  Dual.Record.insert label (field (reflectSymbol label) dual)

-- | The same `Symbol` is used for value lookup in JSON object
-- | and in parsed record value.
fieldProp :: forall a e m r r' s
  . IsSymbol s
  => Cons s a r' r
  => Monad m
  => SProxy s
  -> JsonDual m e a
  -> DualD (Validator m (Validators.Errors (JsonError e))) (Object Json) { | r } a
fieldProp label validator =
  get label ~ field (reflectSymbol label) validator

infix 10 insert as :=

newtypeDual :: forall a e m n. Monad m => Newtype n a => Dual m e a n
newtypeDual = dual (hoistFn wrap) unwrap

mapDual :: forall a b e m t. Monad m => Functor t => (a -> b) -> (b -> a) -> Dual m e (t a) (t b)
mapDual f g = dual (hoistFn (map f)) (map g)

variant ∷
  ∀ e d dl m v.
  Monad m ⇒
  RowToList d dl ⇒
  GDualVariant (Validator m (Validators.Errors (JsonError e))) Argonaut.Json dl d v ⇒
  { | d } →
  JsonDual m e (Variant v)
variant = Duals.Validator.Generic.variant tagWithValue
  where
    tagWithValue ∷ ∀ a s. Monad m ⇒ IsSymbol s ⇒ SProxy s → JsonDual m e a → JsonDual m e a
    tagWithValue fieldSymbol (Dual.Dual (Dual.DualD prs ser))  =
      object >>> tagFields >>> tagged
      where
        tagFields = Dual.Dual $ { t: _, v: _ }
          <$> _.t ~ field "tag" string
          <*> _.v ~ field "value" identity

        tagged =
          let
            fieldName = reflectSymbol fieldSymbol
            ser' = ser >>> { t: fieldName, v: _ }
            prs' = prs <<< hoistFnV \{ t, v } → if fieldName /= t
              then failure ("Incorrect tag: " <> t)
              else valid v
          in
            dual prs' ser'
