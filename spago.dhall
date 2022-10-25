{ name = "coroutines"
, dependencies =
  [ "aff"
  , "bifunctors"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "freet"
  , "identity"
  , "integers"
  , "maybe"
  , "newtype"
  , "parallel"
  , "prelude"
  , "profunctor"
  , "tailrec"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
