module Polyform.Dual.Validators.UrlEncoded
  ( Decoded(..)
  , Dual
  , Errors
  , FieldValueDual
  , array
  , boolean
  , field
  , int
  , number
  , optional
  , query
  , string
  )
  where

import Prelude

import Data.Array (fromFoldable, singleton) as Array
import Data.Foldable (foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.List (List(..)) as List
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Number.Format (toString) as Number.Format
import Data.Profunctor (lcmap, rmap)
import Data.String (joinWith) as String
import Global.Unsafe (unsafeEncodeURIComponent)
import Polyform.Dual (Dual) as Polyform.Dual
import Polyform.Dual (dual)
import Polyform.Dual (parser, serializer) as Dual
import Polyform.Validator as Polyform.Validator
import Polyform.Validators as Polyform.Validators
import Polyform.Validators.UrlEncoded as Validators.UrlEncoded
import Polyform.Validators.UrlEncoded.Parser (Options) as Validators.UrlEncoded.Parser
import Type.Row (type (+))

-- | Because `Map` has quite unexpected instance for `Monoid`
-- | we have to provide our own appending instance.
newtype Decoded = Decoded Validators.UrlEncoded.Decoded
derive instance newtypeDecoded ∷ Newtype Decoded _
derive newtype instance eqDecoded ∷ Eq Decoded

query ∷ ∀ e m s
  . Monad m
  ⇒ Applicative s
  ⇒ Validators.UrlEncoded.Parser.Options
  → Polyform.Dual.Dual
      (Polyform.Validator.Validator m (Errors e)) s String Decoded
query opts = dual
  (rmap Decoded (Validators.UrlEncoded.query opts))
  serializer
  where
    serializer ∷ Decoded → s String
    serializer (Decoded m) =
      pure $ String.joinWith "&" $ Array.fromFoldable $ foldrWithIndex step mempty m
      where
        step key values q =
          foldr (substep key) mempty values <> q
        substep key value subquery =
          List.Cons (unsafeEncodeURIComponent key <> "=" <> unsafeEncodeURIComponent value) subquery

instance semigroupDecoded ∷ Semigroup Decoded where
  append (Decoded d1) (Decoded d2) = Decoded (Map.unionWith append d1 d2)

instance monoidDecoded ∷ Monoid Decoded where
  mempty = Decoded mempty

type Errors e = Polyform.Validators.Errors (Validators.UrlEncoded.Error + e)
type FieldValueDual m s a = Polyform.Dual.Dual (Polyform.Validator.Validator m (Array String)) s (Maybe (Array String)) a
type Dual m s e a = Polyform.Dual.Dual
  (Polyform.Validator.Validator m (Errors e)) s Decoded a

boolean ∷ ∀ m s. Monad m ⇒ Applicative s ⇒ FieldValueDual m s Boolean
boolean = dual
  Validators.UrlEncoded.boolean
  (pure <<< if _ then Just ["on"] else Just ["off"])

string ∷ ∀ m s. Monad m ⇒ Applicative s ⇒ FieldValueDual m s String
string = dual Validators.UrlEncoded.string (pure <<< Just <<< Array.singleton)

number ∷ ∀ m s. Monad m ⇒ Applicative s ⇒ FieldValueDual m s Number
number = dual
  Validators.UrlEncoded.number
  (pure <<< Just <<< Array.singleton <<< Number.Format.toString)

int ∷ ∀ m s. Monad m ⇒ Applicative s ⇒ FieldValueDual m s Int
int = dual
  Validators.UrlEncoded.int
  (pure <<< Just <<< Array.singleton <<< show)

array ∷ ∀ m s. Monad m ⇒ Applicative s ⇒ FieldValueDual m s (Array String)
array = dual
  Validators.UrlEncoded.array
  (pure <<< Just)

optional ∷ ∀ a m s. Monad m ⇒ Applicative s ⇒ FieldValueDual m s a → FieldValueDual m s (Maybe a)
optional d = dual
  (Validators.UrlEncoded.optional (Dual.parser d))
  (case _ of
    Just a → Dual.serializer d a
    Nothing → pure Nothing)

field ∷ ∀ a e m s. Monad m ⇒ Applicative s ⇒ String → FieldValueDual m s a → Dual m s e a
field name d = dual
  (lcmap unwrap $ Validators.UrlEncoded.field name (Dual.parser d))
  (Dual.serializer d >>> map (fromMaybe [] >>> Map.singleton name >>> Decoded))
