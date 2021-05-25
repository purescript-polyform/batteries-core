{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "polyform-batteries"
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
  , "psci-support"
  , "quickcheck"
  , "strings"
  , "test-unit"
  , "typelevel-prelude"
  , "validation"
  , "variant"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
