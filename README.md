# purescript-polyform-batteries-core

Base `Validator`, `Dual` and error structure plus some simple and general validators / duals (for `Number`, `Int` etc.). This lib provides the structure for the whole `polyform-batteries-*` ecosystem.

## Objectives

* Validation should be orthogonal to the presentation layer. Provide error representation strong enough to construct error messages.

* Provide convinent DLS for validators composition. Use only well established interfaces like `Monoid`, `Applicative` and `Category` as a validators DSL without generic approach or custom typeclasses as a base layer.

* Validation pieces should be fully composable. We want to be able to reuse validation like `decimal ∷ String → V e Decimal` accross different validation contexts whether the underling input is urlencoded string or json.

* There should be a way to specify validation and serialization at the same time. Without custom typeclasses. `Dual` from __polyform__ solves this nicely for us.

* Allow to choose error collecting strategy - whether you want to aggregate all errors or short circuit.

