module Polyform.Duals.Validators.Json
  ( Dual
  , JsonDual
  , ObjectDual
  , Object
  , argonaut
  , arrayOf
  , boolean
  , constField
  , decode
  , encode
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
  , (:>)
  )
  where

import Prelude

import Data.Argonaut (Json, fromBoolean, fromNumber, fromObject, fromString, stringify) as Argonaut
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, fromArray, stringify)
import Data.Argonaut.Core (jsonNull)
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Data.Generic.Rep (class Generic, NoArguments)
import Data.Identity (Identity(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Newtype (un, unwrap)
import Data.Semigroup.First (First(..))
import Data.Traversable (traverse)
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
import Prim.RowList (class RowToList)
import Type.Data.Symbol (SProxy)
import Type.Prelude (class IsSymbol, reflectSymbol)
import Type.Row (type (+))

json :: forall e m s
   . Monad m
  => Applicative s
  => Dual m s (JsonDecodingError e) String Json
json = dual
  Validators.Json.json
  (Argonaut.stringify >>> pure)

argonaut :: forall a e m s. Monad m => Applicative s => EncodeJson a => DecodeJson a => JsonDual m s e a
argonaut = dual prs ser
  where
    prs = hoistFnV (\i -> either failure pure (decodeJson i))
    ser = (pure <<< encodeJson)

type Dual m s e a b = Duals.Validator.Dual m s (Validators.Errors e) a b
type JsonDual m s e a = Dual m s (JsonError e) Argonaut.Json a

-- | Because `Foreign.Object` has monoid instance
-- | which performs underlining `append` on values
-- | we have to wrap it up and provide appropriate
-- | wrapping with `First`.
-- |
-- | TODO: Migrate to Object.Builder simialar
-- | to the one used by webrow.
type Object a = Foreign.Object (First a)

object :: forall e m s. Monad m => Applicative s => JsonDual m s e (Object Argonaut.Json)
object = dual
  (map First <$> Validators.Json.object)
  (pure <<< Argonaut.fromObject <<< map runFirst)
  where
    runFirst (First a) = a

int :: forall e m s. Monad m => Applicative s => JsonDual m s e Int
int = dual
  Validators.Json.int
  (pure <<< Argonaut.fromNumber <<< toNumber)

boolean :: forall e m s. Monad m => Applicative s => JsonDual m s e Boolean
boolean = dual
  Validators.Json.boolean
  (pure <<< Argonaut.fromBoolean)

number :: forall e m s. Monad m => Applicative s => JsonDual m s e Number
number = dual
  Validators.Json.number
  (pure <<< Argonaut.fromNumber)

string :: forall e m s. Monad m => Applicative s => JsonDual m s e String
string = dual
  Validators.Json.string
  (pure <<< Argonaut.fromString)

arrayOf :: forall e o m s. Monad m => Applicative s => JsonDual m s e o -> JsonDual m s e (Array o)
arrayOf (Dual.Dual (Dual.DualD prs ser)) =
  dual (Validators.Json.arrayOf prs) (map fromArray <<< traverse ser)

type ObjectDual m s e a = Dual m s (JsonError e) (Object Argonaut.Json) a

-- | This `First` wrapper is necessary because `Object` provides
-- | `Semigroup` instance which based on value `Semigroup`.
-- | We use just left bias union here.
field :: forall a e m s. Monad m => Applicative s => String -> JsonDual m s e a -> ObjectDual m s e a
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
    ser = map (Foreign.singleton label <<< First) <<< fieldSer

-- | Less efficient version than `field` which parses/serializes JSON
-- | object to operate on a field value.
field' :: forall a e m s. Monad m => Monad s => String -> JsonDual m s e a -> JsonDual m s e a
field' label d = field label d <<< object

insert :: forall e l o m prs prs' s ser ser'
  . Row.Cons l o ser ser'
  => Row.Lacks l ser
  => Row.Cons l o prs prs'
  => Row.Lacks l prs
  => IsSymbol l
  => Monad m
  => Applicative s
  => SProxy l
  -> JsonDual m s e o
  -> Dual.Record.Builder
    (Polyform.Validator m (Validators.Errors (JsonError e)))
    s
    (Object Json)
    { | ser'}
    { | prs}
    { | prs'}
insert label dual =
  Dual.Record.insert label (field (reflectSymbol label) dual)

infix 10 insert as :=

-- | TODO:
-- | * alias signature types
constField :: forall e l o m prs prs' s ser ser'
  . Row.Cons l o ser ser'
  => Row.Lacks l ser
  => Row.Cons l o prs prs'
  => Row.Lacks l prs
  => IsSymbol l
  => Monad m
  => Applicative s
  => SProxy l
  -> o
  -> Dual.Record.Builder
    (Polyform.Validator m (Validators.Errors (JsonError e)))
    s
    (Object Json)
    { | ser'}
    { | prs}
    { | prs'}
constField label a = label := (dual prs ser)
  where
    ser = const $ pure jsonNull
    -- | Do we want to really validate this??
    prs = hoistFnMV $ \i -> pure (valid a)
      -- if isNull i
      -- then pure (valid a)
      -- else pure $
      --   failure ("expecting null value as constant encoding")

-- -- -- optionalField :: forall m e a. Monad m => String -> JsonDual m e a -> ObjectDual m e (Maybe a)
-- -- -- optionalField =
-- -- --   dual prs ser
-- -- --   where
-- -- --     DualD fieldPrs fieldSer = unwrap d
-- -- --     prs = hoistFnMV \obj ->
-- -- --       case Foreign.lookup label obj of
-- -- --         Nothing -> pure $ failure ("no field " <> show label <> " in object " <> show ((stringify <<< unwrap) <$> obj))
-- -- --         Just (First j) -> do
-- -- --           res <- runValidator fieldPrs j
-- -- --           pure $ lmap (extendErr label) res
-- -- --     ser = fieldSer >>> First >>> Foreign.singleton label
-- -- --
-- -- -- insertOptional :: forall e m l o prs prs' ser ser'
-- -- --   . Row.Cons l (Maybe o) ser ser'
-- -- --   => Row.Lacks l ser
-- -- --   => Row.Cons l (Maybe o) prs prs'
-- -- --   => Row.Lacks l prs
-- -- --   => IsSymbol l
-- -- --   => Monad m
-- -- --   => SProxy l
-- -- --   -> JsonDual m e o
-- -- --   -> Dual.Record.Builder
-- -- --     (Polyform.Validator m (Validators.Errors (JsonError e)))
-- -- --     (Object Json)
-- -- --     { | ser'}
-- -- --     { | prs}
-- -- --     { | prs'}
-- -- -- insertOptional label dual =
-- -- --   Dual.Record.insert label (optionalField (reflectSymbol label) dual)

variant ::
  forall e d dl m s v
  . Monad m
  ⇒ Monad s
  ⇒ RowToList d dl
  ⇒ GDualVariant (Validator m (Validators.Errors (JsonError e))) s Argonaut.Json dl d v
  ⇒ { | d }
  → JsonDual m s e (Variant v)
variant = Duals.Validator.Generic.variant tagged

sum :: forall a m e rep r s
  . Monad m
  ⇒ Monad s
  => Generic a rep
  => GDualSum (Validator m (Validators.Errors (JsonError e))) s Argonaut.Json rep r
  => { | r }
  -> JsonDual m s e a
sum = Duals.Validator.Generic.sum tagged

tagged :: forall a e l m s. Monad m => Monad s ⇒ IsSymbol l => SProxy l -> JsonDual m s e a -> JsonDual m s e a
tagged label (Dual.Dual (Dual.DualD prs ser))  =
  object >>> tagFields >>> tagged'
  where
    tagFields = Dual.Dual $ { t: _, v: _ }
      <$> _.t ~ field "tag" string
      <*> _.v ~ field "value" identity

    tagged' =
      let
        fieldName = reflectSymbol label
        ser' = ser >>> map { t: fieldName, v: _ }
        prs' = prs <<< hoistFnV \{ t, v } -> if fieldName /= t
          then failure ("Incorrect tag: " <> t)
          else valid v
      in
        dual prs' ser'

on :: forall a l e m r r' s
  . IsSymbol l
  => Monad m
  ⇒ Monad s
  => Row.Cons l a r r'
  => SProxy l
  -> JsonDual m s e a
  -> JsonDual m s e (Variant r)
  -> JsonDual m s e (Variant r')
on label d rest = Dual.Variant.on tagged label d rest

infix 10 on as :>

noArgs :: forall e m s. Monad m => Applicative s ⇒ JsonDual m s e NoArguments
noArgs = Dual.Generic.Sum.noArgs' jsonNull

unit :: forall e m s. Monad m => Applicative s ⇒ JsonDual m s e Unit
unit = Dual.Generic.Sum.unit' jsonNull

decode :: forall a e. JsonDual Identity Identity e a -> Json -> Either (Validators.Errors (JsonError + e)) a
decode dual j =
  unwrap $ unwrap (Duals.Validator.runValidator dual j)

encode :: forall a e. JsonDual Identity Identity e a -> a -> Json
encode dual = un Identity <<< Duals.Validator.runSerializer dual

