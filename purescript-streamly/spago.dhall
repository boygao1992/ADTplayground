{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "streamly"
, dependencies =
    [ "console"
    , "effect"
    , "maybe"
    , "psci-support"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
