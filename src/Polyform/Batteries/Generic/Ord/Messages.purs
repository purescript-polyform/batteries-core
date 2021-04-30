module Polyform.Batteries.Generic.Ord.Messages where

import Prelude

import Polyform.Batteries.Generic.Ord.Validators (NotGreaterThanErr, NotSmallerThanErr, NotInRangeErr)

notGreaterThan :: ∀ a. Show a ⇒ NotGreaterThanErr a → String
notGreaterThan { min, value } = "Given value " <> show value <> " is not greater than " <> show min <> "."

notSmallerThan :: ∀ a. Show a ⇒ NotSmallerThanErr a → String
notSmallerThan { max, value } = "Given value " <> show value <> " is not smaller than " <> show max <> "."

notInRange :: ∀ a. Show a ⇒ NotInRangeErr a → String
notInRange { max, min, value } = "Given value " <> show value <> " is not in expected range (" <> show min <> ", " <> show max <> ")."

