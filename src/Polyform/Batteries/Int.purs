module Polyform.Batteries.Int where

import Prelude
import Data.Int (fromString) as Int
import Polyform.Batteries (Validator, Dual, error) as Batteries
import Polyform.Dual (dual) as Dual
import Polyform.Validator (liftFnMaybe) as Validator
import Type.Prelude (SProxy(..))
import Type.Row (type (+))

_intExpected = SProxy ∷ SProxy "intExpected"

type IntExpected e
  = ( intExpected ∷ String | e )

validator ∷ ∀ e m. Monad m ⇒ Batteries.Validator m (IntExpected + e) String Int
validator = Validator.liftFnMaybe (Batteries.error _intExpected) Int.fromString

dual ∷ ∀ e m. Monad m ⇒ Batteries.Dual m (IntExpected + e) String Int
dual = Dual.dual validator (pure <<< show)
