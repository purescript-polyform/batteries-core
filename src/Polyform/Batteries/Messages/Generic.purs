module Polyform.Batteries.Messages.Generic where

import Prelude

import Polyform.Batteries.Eq.Validators (NotEqualToErr)
import Polyform.Batteries.Generic.Ord.Validators (NotGreaterThanErr, NotSmallerThanErr, NotInRangeErr)

notGreaterThan :: ∀ a. Show a ⇒ NotGreaterThanErr a → String
notGreaterThan { min, value } =
  "Given value " <> show value <> " is not greater than " <> show min <> "."

notSmallerThan :: ∀ a. Show a ⇒ NotSmallerThanErr a → String
notSmallerThan { max, value } =
  "Given value " <> show value <> " is not smaller than " <> show max <> "."

notInRange :: ∀ a. Show a ⇒ NotInRangeErr a → String
notInRange { max, min, value } =
  "Given value " <> show value <> " is not in expected range (" <> show min <> ", " <> show max  <> ")."

notEqualTo :: ∀ a. Show a ⇒ NotEqualToErr a → String
notEqualTo { expected, got } =
  "Expecting value " <> show expected <> " but got " <> show got <> "."

notDifferentThan :: ∀ a. Show a ⇒ a → String
notDifferentThan value =
  "Expecting different value than " <> show value <> "."

-- notOneOf :: ∀ a. Show a ⇒ Array a → String
-- notOneOf { expected, got } =
--   "Expecting value " <> show expected <> " but got " <> show got <> "."
