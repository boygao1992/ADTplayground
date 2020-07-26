{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "purescript-aim"
, dependencies =
    [ "console"
    , "effect"
    , "frp"
    , "functions"
    , "psci-support"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
