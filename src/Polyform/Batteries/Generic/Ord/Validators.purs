module Polyform.Batteries.Generic.Ord.Validators where

import Prelude

import Polyform.Batteries (Validator, error) as Batteries
import Polyform.Validator (check) as Validator
import Prim.Row (class Cons) as Row
import Type.Prelude (class IsSymbol, SProxy)
import Type.Row (type (+))

type NotGreaterThanErr a
  = { value ∷ a, min ∷ a }

greaterThan ∷
  ∀ a err err' l m.
  Row.Cons l (NotGreaterThanErr a) err err' ⇒
  IsSymbol l ⇒
  Ord a ⇒
  Applicative m ⇒
  SProxy l →
  (NotGreaterThanErr a → String) →
  a →
  Batteries.Validator m err' a a
greaterThan l msg min = Validator.check (Batteries.error l msg <<< { min, value: _ }) (_ > min)

type NotSmallerThanErr a
  = { value ∷ a, max ∷ a }

smallerThan ∷
  ∀ a err err' l m.
  Row.Cons l (NotSmallerThanErr a) err err' ⇒
  IsSymbol l ⇒
  Ord a ⇒
  Applicative m ⇒
  SProxy l →
  (NotSmallerThanErr a → String) →
  a →
  Batteries.Validator m err' a a
smallerThan l msg max = Validator.check (Batteries.error l msg <<< { max, value: _ }) (_ < max)

type RangeRow a r
  = ( max ∷ a, min ∷ a | r )

type Range a
  = { | RangeRow a + () }

type NotInRangeErr a
  = { | RangeRow a + ( value ∷ a ) }

inRange ∷
  ∀ a err err' l m.
  Row.Cons l (NotInRangeErr a) err err' ⇒
  IsSymbol l ⇒
  Ord a ⇒
  Applicative m ⇒
  SProxy l →
  (NotInRangeErr a → String) →
  Range a →
  Batteries.Validator m err' a a
inRange l msg { min, max } =
  Validator.check
    (Batteries.error l msg <<< { max, min, value: _ })
    ((||) <$> (_ < min) <*> (max < _))
