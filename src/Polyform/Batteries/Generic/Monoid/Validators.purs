module Polyform.Batteries.Generic.Monoid.Validators where

import Prelude

import Polyform.Batteries (Validator, error) as Batteries
import Polyform.Validator (check) as Validator
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol, SProxy)

isEmpty
  ∷ ∀ a err err' l m
  . Row.Cons l a err err'
  ⇒ IsSymbol l
  ⇒ Eq a
  ⇒ Monoid a
  ⇒ Applicative m
  ⇒ SProxy l
  → Batteries.Validator m err' a a
isEmpty l  = Validator.check (Batteries.error l) (_ == mempty)

isNotEmpty
  ∷ ∀ a err err' l m
  . Row.Cons l Unit err err'
  ⇒ IsSymbol l
  ⇒ Monoid a
  ⇒ Eq a
  ⇒ Applicative m
  ⇒ SProxy l
  → Batteries.Validator m err' a a
isNotEmpty l = Validator.check (const $ Batteries.error l unit) (_ /= mempty)

