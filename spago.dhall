{ name = "polyform-batteries-core"
, license = "BSD-3-Clause"
, dependencies =
  [ "arrays"
  , "decimals"
  , "effect"
  , "enums"
  , "integers"
  , "lazy"
  , "maybe"
  , "numbers"
  , "partial"
  , "polyform"
  , "prelude"
  , "quickcheck"
  , "strings"
  , "test-unit"
  , "typelevel-prelude"
  , "unsafe-coerce"
  , "validation"
  , "variant"
  ]
, packages = ./packages.dhall
, repository = "https://github.com/purescript-polyform/batteries-core.git"
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
