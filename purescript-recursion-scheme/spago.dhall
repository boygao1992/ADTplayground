{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name =
    "my-project"
, dependencies =
    [ "console"
    , "effect"
    , "foreign-object"
    , "free"
    , "functors"
    , "generics-rep"
    , "matryoshka"
    , "newtype"
    , "ordered-collections"
    , "psci-support"
    , "transformers"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
