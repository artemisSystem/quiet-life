{ name = "quiet-life-datagen"
, dependencies =
  [ "aff"
  , "aff-promise"
  , "arrays"
  , "console"
  , "debug"
  , "effect"
  , "either"
  , "exceptions"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "foreign-readwrite"
  , "integers"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "node-buffer"
  , "node-fs"
  , "node-fs-aff"
  , "node-path"
  , "ordered-collections"
  , "polymorphic-vectors"
  , "prelude"
  , "qualified-do"
  , "record"
  , "run"
  , "safe-coerce"
  , "strings"
  , "transformers"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
