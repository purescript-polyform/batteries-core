module Polyform.Batteries.UrlEncoded.Duals
  ( array
  , boolean
  , field
  , Field
  , int
  , number
  , optionalField
  , singleValue
  )
  where

import Prelude

import Data.Array (singleton) as Array
import Data.Map (singleton) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number.Format (toString) as Number.Format
import Polyform.Batteries (Dual) as Batteries
import Polyform.Batteries.UrlEncoded.Query (Decoded(..), Key, Value) as Query
import Polyform.Batteries.UrlEncoded.Types (Dual)
import Polyform.Batteries.UrlEncoded.Validators (IntExpected, NumberExpected, SingleValueExpected, BooleanExpected)
import Polyform.Batteries.UrlEncoded.Validators (array, boolean, field, int, number, optionalField, singleValue) as Validators
import Polyform.Dual (dual)
import Polyform.Dual (parser, serializer) as Dual
import Type.Row (type (+))

type Field m s e b = Batteries.Dual m s e (Maybe Query.Value) b

field
  ∷ ∀ a e m s
  . Monad m
  ⇒ Applicative s
  ⇒ Query.Key
  → Field m s e a
  → Dual m s e Query.Decoded a
field name d = dual
  (Validators.field name (Dual.parser d))
  ( map (Query.Decoded <<< Map.singleton name <<< fromMaybe [])
  <<< Dual.serializer d
  )

boolean ∷ ∀ e m s. Monad m ⇒ Applicative s ⇒ Field m s (BooleanExpected + e) Boolean
boolean = dual
  Validators.boolean
  (pure <<< if _ then Just ["on"] else Just ["off"])

singleValue ∷ ∀ e m s. Monad m ⇒ Applicative s ⇒ Field m s (SingleValueExpected + e) String
singleValue = dual Validators.singleValue (pure <<< Just <<< Array.singleton)

number ∷ ∀ e m s. Monad m ⇒ Applicative s ⇒ Field m s (SingleValueExpected + NumberExpected + e) Number
number = dual
  Validators.number
  (pure <<< Just <<< Array.singleton <<< Number.Format.toString)

int ∷ ∀ e m s. Monad m ⇒ Applicative s ⇒ Field m s (SingleValueExpected + IntExpected + e) Int
int = dual
  Validators.int
  (pure <<< Just <<< Array.singleton <<< show)

array ∷ ∀ e m s. Monad m ⇒ Applicative s ⇒ Field m s e (Array String)
array = dual
  Validators.array
  (pure <<< Just)

optionalField ∷ ∀ a e m s. Monad m ⇒ Applicative s ⇒ Field m s e a → Field m s e (Maybe a)
optionalField d = dual
  (Validators.optionalField (Dual.parser d))
  (case _ of
    Just a → Dual.serializer d a
    Nothing → pure Nothing)

