{ name = "coroutines"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "freet"
  , "parallel"
  , "prelude"
  , "profunctor"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
