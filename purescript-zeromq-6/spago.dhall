{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "zeromq-6"
, dependencies =
    [ "aff"
    , "aff-promise"
    , "console"
    , "effect"
    , "node-buffer"
    , "psci-support"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
