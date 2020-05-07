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

query ∷ ∀ e m
  . Monad m
  ⇒ Validators.UrlEncoded.Parser.Options
  → Polyform.Dual.Dual
      (Polyform.Validator.Validator m (Errors e)) String Decoded
query opts = dual
  (rmap Decoded (Validators.UrlEncoded.query opts))
  serializer
  where
    serializer ∷ Decoded → String
    serializer (Decoded m) =
      String.joinWith "&" $ Array.fromFoldable $ foldrWithIndex step mempty m
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
type FieldValueDual m a = Polyform.Dual.Dual (Polyform.Validator.Validator m (Array String)) (Maybe (Array String)) a
type Dual m e a = Polyform.Dual.Dual
  (Polyform.Validator.Validator m (Errors e)) Decoded a

boolean ∷ ∀ m. Monad m ⇒ FieldValueDual m Boolean
boolean = dual
  Validators.UrlEncoded.boolean
  (if _ then Just ["on"] else Just ["off"])

string ∷ ∀ m. Monad m ⇒ FieldValueDual m String
string = dual Validators.UrlEncoded.string (Just <<< Array.singleton)

number ∷ ∀ m. Monad m ⇒ FieldValueDual m Number
number = dual
  Validators.UrlEncoded.number
  (Just <<< Array.singleton <<< Number.Format.toString)

int ∷ ∀ m. Monad m ⇒ FieldValueDual m Int
int = dual
  Validators.UrlEncoded.int
  (Just <<< Array.singleton <<< show)

array ∷ ∀ m. Monad m ⇒ FieldValueDual m (Array String)
array = dual
  Validators.UrlEncoded.array
  Just

optional ∷ ∀ a m. Monad m ⇒ FieldValueDual m a → FieldValueDual m (Maybe a)
optional d = dual
  (Validators.UrlEncoded.optional (Dual.parser d))
  (identity >=> Dual.serializer d)

field ∷ ∀ a e m. Monad m ⇒ String → FieldValueDual m a → Dual m e a
field name d = dual
  (lcmap unwrap $ Validators.UrlEncoded.field name (Dual.parser d))
  (Dual.serializer d >>> fromMaybe [] >>> Map.singleton name >>> Decoded)
