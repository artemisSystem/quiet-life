{ name = "quiet-life-datagen"
, dependencies =
  [ "aff"
  , "console"
  , "debug"
  , "effect"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "foreign-readwrite"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "prelude"
  , "qualified-do"
  , "record"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "purs/**/*.purs" ]
}
