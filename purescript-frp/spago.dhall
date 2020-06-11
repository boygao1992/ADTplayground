{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "frp"
, dependencies =
  [ "console"
  , "effect"
  , "foreign-object"
  , "psci-support"
  , "refs"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
