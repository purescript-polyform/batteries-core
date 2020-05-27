module Polyform.Json.Duals where

import Prelude

import Data.Argonaut (Json, jsonNull)
import Data.Argonaut (fromArray, fromBoolean, fromNumber, fromObject, fromString) as Argonaut
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Either (note)
import Data.Generic.Rep (class Generic, NoArguments)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe)
import Data.Newtype (un)
import Data.Profunctor (lcmap)
import Data.Profunctor.Choice ((|||))
import Data.Profunctor.Star (Star(..))
import Data.Semigroup.First (First(..))
import Data.Traversable (traverse)
import Data.Validation.Semigroup (invalid)
import Data.Variant (Variant)
import Foreign.Object (Object) as Foreign
import Foreign.Object (singleton) as Object
import Polyform.Dual (Dual(..), DualD(..), hoistParser) as Dual
import Polyform.Dual (dual, (~))
import Polyform.Dual.Generic.Sum (class GDualSum)
import Polyform.Dual.Generic.Sum (noArgs', unit') as Dual.Generic.Sum
import Polyform.Dual.Generic.Variant (class GDualVariant)
import Polyform.Dual.Record (Builder, insert) as Dual.Record
import Polyform.Dual.Variant (on) as Dual.Variant
import Polyform.Json.Validators (ArrayExpected, BooleanExpected, Errors, FieldMissing, JNull, NullExpected, NumberExpected, ObjectExpected, StringExpected, ArgonautError)
import Polyform.Json.Validators (Errors, argonaut, array, arrayOf, boolean, error, field, fromNull, int, liftValidator, null, number, object, optionalField, string) as Json.Validators
import Polyform.Validator (Validator, liftFnV) as Validator
import Polyform.Validator (valid)
import Polyform.Validator.Dual as Validator.Dual
import Polyform.Validator.Dual.Generic (sum, variant) as Validator.Dual.Generic
import Polyform.Validators (Dual) as Validators
import Prim.Row (class Cons, class Lacks) as Row
import Prim.RowList (class RowToList)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Row (type (+))

type Dual m s errs a b = Validator.Dual.Dual m s (Errors errs) a b

-- | Please check `Json.Validator.fromValidator`
fromDual
  ∷ ∀ errs i m o s. Monad m
  ⇒ Validators.Dual m s errs i o
  → Dual m s errs i o
fromDual = Dual.hoistParser Json.Validators.liftValidator

-- | We want to have Monoid for `Object` so we
-- | can compose serializations by monoidal
-- | `append`.
-- | Because `Foreign.Object` has monoid instance
-- | which performs underlining `append` on values
-- | we have to wrap it up and provide appropriate
-- | wrapping with `First`.
type Object a = Foreign.Object (First a)

object
  ∷ ∀ errs m s
  . Monad m
  ⇒ Applicative s
  ⇒ Dual m s (objectExpected ∷ Json | errs) Json (Object Json)
object = dual
  (map First <$> Json.Validators.object)
  (pure <<< Argonaut.fromObject <<< map runFirst)
  where
    runFirst (First a) = a

-- | This `First` wrapper is necessary because `Object` provides
-- | `Semigroup` instance which based on value `Semigroup`.
-- | We use just left bias union here.
field
  ∷ ∀ a e m s
  . Monad m
  ⇒ Applicative s
  ⇒ String
  → Dual m s (FieldMissing + e) Json a
  → Dual m s (FieldMissing + e) (Object Json) a
field label d =
  dual prs ser
  where
    Dual.DualD fieldPrs fieldSer = un Dual.Dual d
    prs = lcmap
      (map (un First))
      (Json.Validators.field label fieldPrs)

    ser = map (Object.singleton label <<< First) <<< fieldSer

optionalField
  ∷ ∀ a err m s. Monad m
  ⇒ Monad s
  ⇒ String
  → Dual m s err Json a
  → Dual m s err (Object Json) (Maybe a)
optionalField label d =
  dual prs ser
  where
    Dual.DualD fieldPrs fieldSer = un Dual.Dual d
    prs = lcmap
      -- | Should we use unsafeCoerce here
      (map (un First))
      (Json.Validators.optionalField label fieldPrs)

    vSer ∷ Maybe a → s Json
    vSer = un Star $ (Star pure ||| Star fieldSer) <<< Star (pure <<< note jsonNull)

    ser = pure <<< (Object.singleton label <<< First) <=< vSer

null
  ∷ ∀ errs m s. Monad m
  ⇒ Applicative s
  ⇒ Dual m s (NullExpected + errs) Json JNull
null = dual
  Json.Validators.null
  (pure <<< Json.Validators.fromNull)

array
  ∷ ∀ errs m s
  . Monad m
  ⇒ Applicative s
  ⇒ Dual m s (ArrayExpected + errs) Json (Array Json)
array = dual
  Json.Validators.array
  (pure <<< Argonaut.fromArray)

arrayOf
  ∷ ∀ e o m s
  . Monad m
  ⇒ Applicative s
  ⇒ Dual m s (ArrayExpected + e) Json o
  → Dual m s (ArrayExpected + e) Json (Array o)
arrayOf (Dual.Dual (Dual.DualD prs ser)) =
  dual (Json.Validators.arrayOf prs) (map Argonaut.fromArray <<< traverse ser)

int
  ∷ ∀ errs m s
  . Monad m
  ⇒ Applicative s
  ⇒ Dual m s (intExpected ∷ Json | errs) Json Int
int = dual
  Json.Validators.int
  (pure <<< Argonaut.fromNumber <<< Int.toNumber)

boolean
  ∷ ∀ errs m s
  . Monad m
  ⇒ Applicative s
  ⇒ Dual m s (BooleanExpected + errs) Json Boolean
boolean = dual
  Json.Validators.boolean
  (pure <<< Argonaut.fromBoolean)

number
  ∷ ∀ errs m s
  . Monad m
  ⇒ Applicative s
  ⇒ Dual m s (NumberExpected + errs) Json Number
number = dual
  Json.Validators.number
  (pure <<< Argonaut.fromNumber)

string
  ∷ ∀ errs m s
  . Monad m
  ⇒ Applicative s
  ⇒ Dual m s (StringExpected + errs) Json String
string = dual
  Json.Validators.string
  (pure <<< Argonaut.fromString)

insert ∷ ∀ e l o m prs prs' s ser ser'
  . Row.Cons l o ser ser'
  ⇒ Row.Lacks l ser
  ⇒ Row.Cons l o prs prs'
  ⇒ Row.Lacks l prs
  ⇒ IsSymbol l
  ⇒ Monad m
  ⇒ Applicative s
  ⇒ SProxy l
  → Dual m s (FieldMissing + e) Json o
  → Dual.Record.Builder
    (Validator.Validator m (Json.Validators.Errors (FieldMissing + e)))
    s
    (Object Json)
    { | ser'}
    { | prs}
    { | prs'}
insert label dual =
  Dual.Record.insert label (field (reflectSymbol label) dual)

infix 10 insert as :=

insertOptional ∷ ∀ e m l o prs prs' s ser ser'
  . Row.Cons l (Maybe o) ser ser'
  ⇒ Row.Lacks l ser
  ⇒ Row.Cons l (Maybe o) prs prs'
  ⇒ Row.Lacks l prs
  ⇒ IsSymbol l
  ⇒ Monad m
  ⇒ Monad s
  ⇒ SProxy l
  → Dual m s e Json o
  → Dual.Record.Builder
    (Validator.Validator m (Json.Validators.Errors e))
    -- (Polyform.Validator m (Validators.Errors (JsonError e)))
    s
    (Object Json)
    { | ser'}
    { | prs}
    { | prs'}
insertOptional label dual =
  Dual.Record.insert label (optionalField (reflectSymbol label) dual)

infix 10 insertOptional as :=?

type CoproductErrors e = (IncorrectVariantTag + StringExpected + FieldMissing + ObjectExpected + e)

variant
  ∷ ∀ e d dl m s v
  . Monad m
  ⇒ Monad s
  ⇒ RowToList d dl
  ⇒ GDualVariant (Validator.Validator m (Json.Validators.Errors (CoproductErrors + e))) s Json dl d v
  ⇒ { | d }
  → Dual m s (CoproductErrors + e) Json (Variant v)
variant = Validator.Dual.Generic.variant tagged

sum ∷ ∀ a m e rep r s
  . Monad m
  ⇒ Monad s
  ⇒ Generic a rep
  ⇒ GDualSum (Validator.Validator m (Json.Validators.Errors (CoproductErrors + e))) s Json rep r
  ⇒ { | r }
  → Dual m s (CoproductErrors + e) Json a
sum = Validator.Dual.Generic.sum tagged

_incorrectVariantTag = SProxy ∷ SProxy "incorrectVariantTag"

type IncorrectVariantTag e = (incorrectVariantTag ∷ String | e)

tagged
  ∷ ∀ a e l m s. Monad m
  ⇒ Monad s
  ⇒ IsSymbol l
  ⇒ SProxy l
  → Dual m s (CoproductErrors + e) Json a
  → Dual m s (CoproductErrors + e) Json a
tagged label (Dual.Dual (Dual.DualD prs ser))  =
  object >>> tagFields >>> tagged'
  where
    tagFields = Dual.Dual $ { t: _, v: _ }
      <$> _.t ~ field "tag" string
      <*> _.v ~ field "value" identity

    tagged' =
      let
        fieldName = reflectSymbol label
        ser' = map { t: fieldName, v: _ } <<< ser
        prs' = prs <<< Validator.liftFnV \{ t, v } → if fieldName /= t
          then invalid $ Json.Validators.error _incorrectVariantTag t
          else valid v
      in
        dual prs' ser'

on ∷ ∀ a l e m r r' s
  . IsSymbol l
  ⇒ Monad m
  ⇒ Monad s
  ⇒ Row.Cons l a r r'
  ⇒ SProxy l
  → Dual m s (CoproductErrors + e) Json a
  → Dual m s (CoproductErrors + e) Json (Variant r)
  → Dual m s (CoproductErrors + e) Json (Variant r')
on label d rest = Dual.Variant.on tagged label d rest

infix 10 on as :>

noArgs ∷ ∀ e m s. Monad m ⇒ Applicative s ⇒ Dual m s e Json NoArguments
noArgs = Dual.Generic.Sum.noArgs' jsonNull

unit ∷ ∀ e m s. Monad m ⇒ Applicative s ⇒ Dual m s e Json Unit
unit = Dual.Generic.Sum.unit' jsonNull

argonaut ∷ ∀ a e m s. Monad m ⇒ Applicative s ⇒ EncodeJson a ⇒ DecodeJson a ⇒ Dual m s (ArgonautError + e) Json a
argonaut = dual Json.Validators.argonaut ser
  where
    ser = (pure <<< encodeJson)


-- -- | TODO:
-- -- | * alias signature types
-- constField ∷ ∀ e l o m prs prs' s ser ser'
--   . Row.Cons l o ser ser'
--   ⇒ Row.Lacks l ser
--   ⇒ Row.Cons l o prs prs'
--   ⇒ Row.Lacks l prs
--   ⇒ IsSymbol l
--   ⇒ Monad m
--   ⇒ Applicative s
--   ⇒ SProxy l
--   → o
--   → Dual.Record.Builder
--     (Polyform.Validator m (Validators.Errors (JsonError e)))
--     s
--     (Object Json)
--     { | ser'}
--     { | prs}
--     { | prs'}
-- constField label a = label := (dual prs ser)
--   where
--     ser = const $ pure jsonNull
--     -- | Do we want to really validate this??
--     prs = liftFnMV $ \i → pure (valid a)
--       -- if isNull i
--       -- then pure (valid a)
--       -- else pure $
--       --   failure ("expecting null value as constant encoding")
-- 

-- decode ∷ ∀ a e. JsonDual Identity Identity e a → Json → Either (Validators.Errors (JsonError + e)) a
-- decode dual j =
--   unwrap $ unwrap (Validator.Dual.runValidator dual j)
-- 
-- encode ∷ ∀ a e. JsonDual Identity Identity e a → a → Json
-- encode dual = un Identity <<< Validator.Dual.runSerializer dual
-- 
