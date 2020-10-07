{ name = "coroutines"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "freet"
  , "parallel"
  , "profunctor"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
