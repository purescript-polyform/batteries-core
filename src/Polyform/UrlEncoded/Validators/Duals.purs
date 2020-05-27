module Polyform.UrlEncoded.Validators.Duals
  ( array
  , boolean
  , field
  , Field
  , int
  , module Validators
  , nonBlank
  , number
  , optional
  , query
  , singleString
  )
  where

import Prelude

import Data.Array (singleton) as Array
import Data.Foldable (foldMap)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.FormURLEncoded (FormURLEncoded(..))
import Data.FormURLEncoded (toArray) as FormURLEncoded
import Data.Map (singleton) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (toString) as Number.Format
import Data.String (joinWith) as String
import Data.Tuple (Tuple(..))
import Global.Unsafe (unsafeEncodeURIComponent)
import Polyform.Data.Array.Builder (build, cons) as Array.Builder
import Polyform.Dual (dual)
import Polyform.Dual (parser, serializer) as Dual
import Polyform.UrlEncoded.Query (Decoded(..), Key, Value) as Query
import Polyform.UrlEncoded.Query (Options) as UrlEncoded.Query
import Polyform.UrlEncoded.Validators (_booleanExpected, _fieldError, _intExpected, _nonBlankExpected, _numberExpected, _queryParseError, _singleStringExpected) as Validators
import Polyform.UrlEncoded.Validators as UrlEncoded.Validators
import Polyform.Validators (Dual) as Validators

query ∷ ∀ errs fieldErrs m s
  . Monad m
  ⇒ Applicative s
  ⇒ UrlEncoded.Query.Options
  → Validators.Dual m s (UrlEncoded.Validators.Error fieldErrs errs) String Query.Decoded
query opts = dual (UrlEncoded.Validators.query opts) serializer
  where
    serializer (Query.Decoded m) =
      pure <<< unsafeEncode <<< FormURLEncoded <<< Array.Builder.build <<< foldMapWithIndex step $ m
      where
        step key [] = Array.Builder.cons (Tuple key Nothing)
        step key values = foldMap (\v → Array.Builder.cons (Tuple key (Just v))) values

    -- | TODO: Make this safe by dropping unsupported codeunits:
    -- |
    -- | * https://stackoverflow.com/questions/16868415/encodeuricomponent-throws-an-exception
    -- |
    -- | * urlPart = urlPart.replace(/[\ud800-\udfff]/g, '');

    unsafeEncode ∷ FormURLEncoded → String
    unsafeEncode = String.joinWith "&" <<< map encodePart <<< FormURLEncoded.toArray
      where
        encodePart = case _ of
          Tuple k Nothing -> unsafeEncodeURIComponent k
          Tuple k (Just v) -> unsafeEncodeURIComponent k <> "=" <> unsafeEncodeURIComponent v

field
  ∷ ∀ a e m s
  . Monad m
  ⇒ Applicative s
  ⇒ Query.Key
  → Field m s e a
  → Validators.Dual
      m
      s
      (urlEncodedQueryParseError ∷ String, urlEncodedFieldError ∷ UrlEncoded.Validators.FieldError e | e)
      Query.Decoded
      a
field name d = dual
  (UrlEncoded.Validators.field name (Dual.parser d))
  (Dual.serializer d >>> map (fromMaybe [] >>> Map.singleton name >>> Query.Decoded))

type Field m s e b = Validators.Dual m s e (Maybe Query.Value) b

boolean ∷ ∀ e m s. Monad m ⇒ Applicative s ⇒ Field m s (urlEncodedBooleanExpected ∷ Query.Value | e) Boolean
boolean = dual
  UrlEncoded.Validators.boolean
  (pure <<< if _ then Just ["on"] else Just ["off"])

singleString ∷ ∀ e m s. Monad m ⇒ Applicative s ⇒ Field m s (urlEncodedSingleStringExpected ∷ Maybe Query.Value | e) String
singleString = dual UrlEncoded.Validators.singleString (pure <<< Just <<< Array.singleton)

nonBlank ∷ ∀ e m s. Monad m ⇒ Applicative s ⇒ Field m s (urlEncodedSingleStringExpected ∷ Maybe Query.Value, urlEncodedNonBlankExpected ∷ Unit | e) String
nonBlank = dual UrlEncoded.Validators.nonBlank (pure <<< Just <<< Array.singleton)

number ∷ ∀ e m s. Monad m ⇒ Applicative s ⇒ Field m s (urlEncodedSingleStringExpected ∷ Maybe Query.Value, urlEncodedNumberExpected ∷ String | e) Number
number = dual
  UrlEncoded.Validators.number
  (pure <<< Just <<< Array.singleton <<< Number.Format.toString)

int ∷ ∀ e m s. Monad m ⇒ Applicative s ⇒ Field m s (urlEncodedSingleStringExpected ∷ Maybe Query.Value, urlEncodedIntExpected ∷ String | e) Int
int = dual
  UrlEncoded.Validators.int
  (pure <<< Just <<< Array.singleton <<< show)

array ∷ ∀ e m s. Monad m ⇒ Applicative s ⇒ Field m s e (Array String)
array = dual
  UrlEncoded.Validators.array
  (pure <<< Just)

optional ∷ ∀ a e m s. Monad m ⇒ Applicative s ⇒ Field m s e a → Field m s e (Maybe a)
optional d = dual
  (UrlEncoded.Validators.optional (Dual.parser d))
  (case _ of
    Just a → Dual.serializer d a
    Nothing → pure Nothing)

