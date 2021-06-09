{ name = "polyform-batteries"
, dependencies =
  [ "console"
  , "debug"
  , "decimals"
  , "effect"
  , "form-urlencoded"
  , "numbers"
  , "polyform"
  , "prelude"
  , "psci-support"
  , "record-extra"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
