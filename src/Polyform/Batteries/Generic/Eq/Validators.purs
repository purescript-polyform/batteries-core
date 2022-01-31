module Polyform.Batteries.Generic.Eq.Validators where

import Prelude

import Data.Array (elem) as Array
import Polyform.Batteries (Validator', error) as Batteries
import Polyform.Validator (check) as Validator
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol)
import Type.Proxy (Proxy)

type NotEqualToErr a
  = { expected ∷ a, got ∷ a }

equalTo ∷
  ∀ a err err' l m.
  Row.Cons l (NotEqualToErr a) err err' ⇒
  IsSymbol l ⇒
  Eq a ⇒
  Applicative m ⇒
  Proxy l →
  (NotEqualToErr a → String) →
  a →
  Batteries.Validator' m err' a a
equalTo l msg expected = Validator.check (Batteries.error l msg <<< { expected, got: _ }) (_ == expected)

differentThan ∷
  ∀ a err err' l m.
  Row.Cons l a err err' ⇒
  IsSymbol l ⇒
  Eq a ⇒
  Applicative m ⇒
  Proxy l →
  (a → String) →
  a →
  Batteries.Validator' m err' a a
differentThan l msg a = Validator.check (const $ Batteries.error l msg a) (_ /= a)

type NotOneOfErr a
  = { expected ∷ Array a, got ∷ a }

oneOf ∷
  ∀ a err err' l m.
  Row.Cons l (NotOneOfErr a) err err' ⇒
  IsSymbol l ⇒
  Eq a ⇒
  Applicative m ⇒
  Proxy l →
  (NotOneOfErr a → String) →
  Array a →
  Batteries.Validator' m err' a a
oneOf l msg arr = Validator.check (Batteries.error l msg <<< { expected: arr, got: _ }) (_ `Array.elem` arr)

type NotMissingFromErr a
  = { unexpected ∷ Array a, got ∷ a }

missingFrom ∷
  ∀ a err err' l m.
  Row.Cons l (NotMissingFromErr a) err err' ⇒
  IsSymbol l ⇒
  Eq a ⇒
  Applicative m ⇒
  Proxy l →
  (NotMissingFromErr a → String) →
  Array a →
  Batteries.Validator' m err' a a
missingFrom l msg arr = Validator.check (Batteries.error l msg <<< { unexpected: arr, got: _ }) (not <<< flip Array.elem arr)
