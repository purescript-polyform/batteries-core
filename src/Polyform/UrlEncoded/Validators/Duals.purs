module Polyform.UrlEncoded.Validators.Duals
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
import Polyform.UrlEncoded.Parser (Options) as UrlEncoded.Parser
import Polyform.UrlEncoded.Types (Decoded) as UrlEncoded.Types
import Polyform.UrlEncoded.Validators as UrlEncoded.Validators
import Polyform.Validator as Polyform.Validator
import Polyform.Validators as Polyform.Validators
import Type.Row (type (+))

-- | Because `Map` has quite unexpected instance for `Monoid`
-- | we have to provide our own appending instance.
newtype Decoded = Decoded UrlEncoded.Types.Decoded
derive instance newtypeDecoded ∷ Newtype Decoded _
derive newtype instance eqDecoded ∷ Eq Decoded

query ∷ ∀ e m s
  . Monad m
  ⇒ Applicative s
  ⇒ UrlEncoded.Parser.Options
  → Polyform.Dual.Dual
      (Polyform.Validator.Validator m (Errors e)) s String Decoded
query opts = dual
  (rmap Decoded (UrlEncoded.Validators.query opts))
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

type Errors e = Polyform.Validators.Errors (UrlEncoded.Validators.Error + e)
type FieldValueDual m s a = Polyform.Dual.Dual (Polyform.Validator.Validator m (Array String)) s (Maybe (Array String)) a
type Dual m s e a = Polyform.Dual.Dual
  (Polyform.Validator.Validator m (Errors e)) s Decoded a

boolean ∷ ∀ m s. Monad m ⇒ Applicative s ⇒ FieldValueDual m s Boolean
boolean = dual
  UrlEncoded.Validators.boolean
  (pure <<< if _ then Just ["on"] else Just ["off"])

string ∷ ∀ m s. Monad m ⇒ Applicative s ⇒ FieldValueDual m s String
string = dual UrlEncoded.Validators.string (pure <<< Just <<< Array.singleton)

number ∷ ∀ m s. Monad m ⇒ Applicative s ⇒ FieldValueDual m s Number
number = dual
  UrlEncoded.Validators.number
  (pure <<< Just <<< Array.singleton <<< Number.Format.toString)

int ∷ ∀ m s. Monad m ⇒ Applicative s ⇒ FieldValueDual m s Int
int = dual
  UrlEncoded.Validators.int
  (pure <<< Just <<< Array.singleton <<< show)

array ∷ ∀ m s. Monad m ⇒ Applicative s ⇒ FieldValueDual m s (Array String)
array = dual
  UrlEncoded.Validators.array
  (pure <<< Just)

optional ∷ ∀ a m s. Monad m ⇒ Applicative s ⇒ FieldValueDual m s a → FieldValueDual m s (Maybe a)
optional d = dual
  (UrlEncoded.Validators.optional (Dual.parser d))
  (case _ of
    Just a → Dual.serializer d a
    Nothing → pure Nothing)

field ∷ ∀ a e m s. Monad m ⇒ Applicative s ⇒ String → FieldValueDual m s a → Dual m s e a
field name d = dual
  (lcmap unwrap $ UrlEncoded.Validators.field name (Dual.parser d))
  (Dual.serializer d >>> map (fromMaybe [] >>> Map.singleton name >>> Decoded))
