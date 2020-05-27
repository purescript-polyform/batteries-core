# purescript-polyform-validators

Set of useful validators which are built on top of `Validator` and `Dual` from __purescript-polyform__.

## Dependencies

Currently we use "batteries included" approach and use __decimal.js__ and __moment.js__ as a part of this lib dependencies. Additionally we provide here basic validators for both urlencoded and json encoded data plus some extras like `Decimal`. It is possible that in the future we split this into smaller libs.

## Objectives

* Validation should be orthogonal to the presentation layer. Provide error representation strong enough to construct error messages.

* Provide convinent DLS for validators composition. Use only well established interfaces like `Monoid`, `Applicative` and `Category` as a validators DSL without generic approach or custom typeclasses as a base layer.

* Validation pieces should be fully composable. We want to be able to reuse validation like `decimal ∷ String → V e Decimal` accross different validation contexts whether the underling input is urlencoded string or json.

* There should be a way to specify validation and serialization at the same time. Without custom typeclasses. `Dual` from __polyform__ solves this nicely for us.

* Allow to choose error collecting strategy - whether you want to aggregate all errors or short circuit.

