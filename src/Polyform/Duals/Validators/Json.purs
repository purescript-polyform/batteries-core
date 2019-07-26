module Polyform.Duals.Validators.Json
  ( Dual
  , JsonDual
  , ObjectDual
  , Object
  , argonaut
  , boolean
  , int
  , insert
  , json
  , noArgs
  , number
  , object
  , on
  , field
  , string
  , sum
  , unit
  , variant
  , (:=)
  )
  where

import Prelude

import Data.Argonaut (Json, fromBoolean, fromNumber, fromObject, fromString, stringify) as Argonaut
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, fromArray, stringify)
import Data.Argonaut.Core (jsonNull)
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Generic.Rep (class Generic, NoArguments)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup.First (First(..))
import Data.Variant (Variant)
import Foreign.Object (Object, lookup, singleton) as Foreign
import Polyform.Dual (Dual(..), DualD(..)) as Dual
import Polyform.Dual (DualD(..), dual, (~))
import Polyform.Dual.Generic (class GDualVariant)
import Polyform.Dual.Generic.Sum (class GDualSum)
import Polyform.Dual.Generic.Sum (noArgs', unit') as Dual.Generic.Sum
import Polyform.Dual.Record (Builder, insert) as Dual.Record
import Polyform.Dual.Variant (on) as Dual.Variant
import Polyform.Duals.Validator as Duals.Validator
import Polyform.Duals.Validator.Generic (sum, variant) as Duals.Validator.Generic
import Polyform.Validator (Validator) as Polyform
import Polyform.Validator (Validator, hoistFnMV, hoistFnV, runValidator, valid)
import Polyform.Validators (Errors) as Validators
import Polyform.Validators.Json (JsonDecodingError, JsonError, extendErr, failure)
import Polyform.Validators.Json (arrayOf, boolean, int, json, number, object, string) as Validators.Json
import Prim.Row (class Cons, class Lacks) as Row
import Prim.Row (class Union)
import Prim.RowList (class RowToList)
import Type.Data.Symbol (SProxy)
import Type.Prelude (class IsSymbol, reflectSymbol)

json :: forall e m
   . Monad m
  => Dual m (JsonDecodingError e) String Json
json = dual
  Validators.Json.json
  Argonaut.stringify

argonaut ∷ ∀ a e m. Applicative m ⇒ EncodeJson a ⇒ DecodeJson a ⇒ JsonDual m e a
argonaut = dual prs ser
  where
    prs = hoistFnV (\i → either failure pure (decodeJson i))
    ser = encodeJson

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

arrayOf ∷ ∀ m e o. Monad m ⇒ JsonDual m e o → JsonDual m e (Array o)
arrayOf (Dual.Dual (Dual.DualD prs ser)) =
  dual (Validators.Json.arrayOf prs) (fromArray <<< map ser)

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

-- | Less efficient version than `field` which parses/serializes JSON
-- | object to operate on field value.
field' :: forall m e a. Monad m => String -> JsonDual m e a -> JsonDual m e a
field' label d = object >>> field label d

insert ∷ ∀ e m l o prs prs' ser ser'
  . Row.Cons l o ser ser'
  ⇒ Row.Lacks l ser
  ⇒ Row.Cons l o prs prs'
  ⇒ Row.Lacks l prs
  ⇒ IsSymbol l
  ⇒ Monad m
  ⇒ SProxy l
  → JsonDual m e o
  → Dual.Record.Builder
    (Polyform.Validator m (Validators.Errors (JsonError e)))
    (Object Json)
    { | ser'}
    { | prs}
    { | prs'}
insert label dual =
  Dual.Record.insert label (field (reflectSymbol label) dual)

infix 10 insert as :=

-- optionalField :: ∀ m e a. Monad m ⇒ String → JsonDual m e a → ObjectDual m e (Maybe a)
-- optionalField =
--   dual prs ser
--   where
--     DualD fieldPrs fieldSer = unwrap d
--     prs = hoistFnMV \obj ->
--       case Foreign.lookup label obj of
--         Nothing -> pure $ failure ("no field " <> show label <> " in object " <> show ((stringify <<< unwrap) <$> obj))
--         Just (First j) -> do
--           res <- runValidator fieldPrs j
--           pure $ lmap (extendErr label) res
--     ser = fieldSer >>> First >>> Foreign.singleton label
--
-- insertOptional ∷ ∀ e m l o prs prs' ser ser'
--   . Row.Cons l (Maybe o) ser ser'
--   ⇒ Row.Lacks l ser
--   ⇒ Row.Cons l (Maybe o) prs prs'
--   ⇒ Row.Lacks l prs
--   ⇒ IsSymbol l
--   ⇒ Monad m
--   ⇒ SProxy l
--   → JsonDual m e o
--   → Dual.Record.Builder
--     (Polyform.Validator m (Validators.Errors (JsonError e)))
--     (Object Json)
--     { | ser'}
--     { | prs}
--     { | prs'}
-- insertOptional label dual =
--   Dual.Record.insert label (optionalField (reflectSymbol label) dual)

variant ∷
  ∀ e d dl m v.
  Monad m ⇒
  RowToList d dl ⇒
  GDualVariant (Validator m (Validators.Errors (JsonError e))) Argonaut.Json dl d v ⇒
  { | d } →
  JsonDual m e (Variant v)
variant = Duals.Validator.Generic.variant tagged

sum ∷ ∀ a m e rep r
  . Monad m
  ⇒ Generic a rep
  ⇒ GDualSum (Validator m (Validators.Errors (JsonError e))) Argonaut.Json rep r
  ⇒ { | r }
  → JsonDual m e a
sum = Duals.Validator.Generic.sum tagged

tagged ∷ ∀ a e m s. Monad m ⇒ IsSymbol s ⇒ SProxy s → JsonDual m e a → JsonDual m e a
tagged label (Dual.Dual (Dual.DualD prs ser))  =
  object >>> tagFields >>> tagged'
  where
    tagFields = Dual.Dual $ { t: _, v: _ }
      <$> _.t ~ field "tag" string
      <*> _.v ~ field "value" identity

    tagged' =
      let
        fieldName = reflectSymbol label
        ser' = ser >>> { t: fieldName, v: _ }
        prs' = prs <<< hoistFnV \{ t, v } → if fieldName /= t
          then failure ("Incorrect tag: " <> t)
          else valid v
      in
        dual prs' ser'

on ∷ ∀ a l lr e m r r'
  . Union r lr r'
  ⇒ IsSymbol l
  ⇒ Row.Cons l a () lr
  ⇒ Monad m
  ⇒ Row.Cons l a r r'
  ⇒ SProxy l
  → JsonDual m e a
  → JsonDual m e (Variant r)
  → JsonDual m e (Variant r')
on label d rest = Dual.Variant.on tagged label d rest

noArgs ∷ ∀ e m. Monad m ⇒ JsonDual m e NoArguments
noArgs = Dual.Generic.Sum.noArgs' jsonNull

unit ∷ ∀ e m. Monad m ⇒ JsonDual m e Unit
unit = Dual.Generic.Sum.unit' jsonNull

