module Polyform.Batteries.Generic.Monoid.Messages where

import Prelude

emptyExpected ∷ ∀ a. Monoid a ⇒ Show a ⇒ a → String
emptyExpected a = "Expecting: " <> show (mempty ∷ a) <> " but got " <> show a <> "."

notEmptyExpected ∷ ∀ a. Monoid a ⇒ Show a ⇒ a → String
notEmptyExpected _ = "Expecting value different than: " <> show (mempty ∷ a) <> "."
