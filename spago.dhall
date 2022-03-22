{ name = "coroutines"
, dependencies =
  [ "aff"
  , "bifunctors"
  , "console"
  , "effect"
  , "either"
  , "freet"
  , "identity"
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
