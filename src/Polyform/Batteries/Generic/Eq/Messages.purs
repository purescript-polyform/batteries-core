module Polyform.Batteries.Generic.Eq.Messages where

import Prelude

import Polyform.Batteries.Generic.Eq.Validators (NotEqualToErr, NotOneOfErr, NotMissingFromErr)

notEqualTo ∷ ∀ a. Show a ⇒ NotEqualToErr a → String
notEqualTo { expected, got } = "Expecting value " <> show expected <> " but got " <> show got <> "."

notDifferentThan ∷ ∀ a. Show a ⇒ a → String
notDifferentThan value = "Expecting different value than " <> show value <> "."

notOneOf ∷ ∀ a. Show a ⇒ NotOneOfErr a → String
notOneOf { expected, got } =
  "Expecting value " <> show expected <> " but got " <> show got <> "."

notMissingFrom ∷ ∀ a. Show a ⇒ NotMissingFromErr a → String
notMissingFrom { unexpected, got } =
  "Expecting value outside from the set: " <> show unexpected <> " but got " <> show got <> "."
