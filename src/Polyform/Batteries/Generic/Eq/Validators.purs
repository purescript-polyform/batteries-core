module Polyform.Batteries.Eq.Validators where

import Prelude

import Data.Array (elem) as Array
import Polyform.Batteries (Validator, error) as Batteries
import Polyform.Validator (check) as Validator
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol, SProxy(..))

_notEqualTo = SProxy ∷ SProxy "notEqual"

type NotEqualToErr a = { expected ∷ a, got ∷ a }

equalTo
  ∷ ∀ a err err' l m
  . Row.Cons l (NotEqualToErr a) err err'
  ⇒ IsSymbol l
  ⇒ Eq a
  ⇒ Applicative m
  ⇒ SProxy l
  → a
  → Batteries.Validator m err' a a
equalTo l expected = Validator.check (Batteries.error l <<< { expected, got: _ }) (_ == expected)

differentThan
  ∷ ∀ a err err' l m
  . Row.Cons l a err err'
  ⇒ IsSymbol l
  ⇒ Eq a
  ⇒ Applicative m
  ⇒ SProxy l
  → a
  → Batteries.Validator m err' a a
differentThan l a = Validator.check (const $ Batteries.error l a) (_ /= a)

oneOf
  ∷ ∀ a err err' l m
  . Row.Cons l (Array a) err err'
  ⇒ IsSymbol l
  ⇒ Eq a
  ⇒ Applicative m
  ⇒ SProxy l
  → Array a
  → Batteries.Validator m err' a a
oneOf l arr = Validator.check (const $ Batteries.error l arr) (_ `Array.elem` arr)

missingFrom
  ∷ ∀ a err err' l m
  . Row.Cons l (Array a) err err'
  ⇒ IsSymbol l
  ⇒ Eq a
  ⇒ Applicative m
  ⇒ SProxy l
  → Array a
  → Batteries.Validator m err' a a
missingFrom l arr = Validator.check (const $ Batteries.error l arr) (not <<< flip Array.elem arr)
