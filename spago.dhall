{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "formatting"
, dependencies = [ "avar", "console", "effect", "psci-support", "test-unit" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
