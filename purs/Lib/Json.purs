module Lib.Json where

import Prelude

import Foreign (Foreign)

foreign import unsafeFormatJson ∷ Foreign → String
