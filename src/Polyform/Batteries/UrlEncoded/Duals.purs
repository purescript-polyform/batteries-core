-- | You can use `Batteries.Integer.dual` or `Batteries.Number.dual` or any
-- | other `String` based dual as value dual here.
module Polyform.Batteries.UrlEncoded.Duals
  ( array
  , boolean
  , Field
  , optional
  , required
  , value
  ) where

import Prelude
import Data.Array (singleton) as Array
import Data.Map (singleton) as Map
import Data.Maybe (Maybe(..))
import Polyform.Batteries (Dual) as Batteries
import Polyform.Batteries.UrlEncoded.Query (Decoded(..), Key, Value) as Query
import Polyform.Batteries.UrlEncoded.Types (Dual)
import Polyform.Batteries.UrlEncoded.Validators (BooleanExpected, MissingValue)
import Polyform.Batteries.UrlEncoded.Validators (array, boolean, optional, required, value) as Validators
import Polyform.Dual (dual)
import Polyform.Dual (parser, serializer) as Dual
import Type.Row (type (+))

type Field m e b
  = Batteries.Dual m e (Maybe Query.Value) b

type SingleField m e b
  = Batteries.Dual m e String b

type MultiField m e b
  = Batteries.Dual m e (Array String) b

optional ∷
  ∀ a e m.
  Monad m ⇒
  Query.Key →
  SingleField m e a →
  Dual m e Query.Decoded (Maybe a)
optional name d = dual validator serializer
  where
  validator = Validators.optional name (Dual.parser d)

  serializer = case _ of
    Just v → do
      i ← Dual.serializer d v
      pure (Query.Decoded (Map.singleton name [ i ]))
    Nothing → pure (Query.Decoded (Map.singleton name []))

required ∷
  ∀ a e m.
  Monad m ⇒
  Query.Key →
  SingleField m (MissingValue + e) a →
  Dual m (MissingValue + e) Query.Decoded a
required name d = dual validator serializer
  where
  validator = Validators.required name (Dual.parser d)

  serializer =
    map (Query.Decoded <<< Map.singleton name <<< Array.singleton)
      <<< Dual.serializer d

value ∷ ∀ e m. Monad m ⇒ Field m (MissingValue + e) String
value = dual Validators.value (pure <<< Just <<< Array.singleton)

boolean ∷ ∀ e m. Monad m ⇒ Field m (BooleanExpected + e) Boolean
boolean =
  dual
    Validators.boolean
    (pure <<< if _ then Just [ "on" ] else Just [ "off" ])

array ∷ ∀ e m. Monad m ⇒ Field m e (Array String)
array =
  dual
    Validators.array
    (pure <<< Just)
