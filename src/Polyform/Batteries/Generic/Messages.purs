module Polyform.Batteries.Generic.Messages
  ( placeholder
  , module Exports
  ) where

import Polyform.Batteries.Generic.Eq.Messages (notEqualTo, notDifferentThan, notOneOf, notMissingFrom) as Exports
import Polyform.Batteries.Generic.Ord.Messages (notGreaterThan, notSmallerThan, notInRange) as Exports
import Polyform.Batteries.Generic.Monoid.Messages (emptyExpected, notEmptyExpected) as Exports

placeholder ∷ ∀ a. a → String
placeholder _ = "Given value is not allowed"
