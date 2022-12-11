module Lib.Json where

import Effect (Effect)
import Foreign (Foreign)

foreign import unsafeFormatJson ∷ Foreign → String

foreign import parseJson ∷ String → Effect Foreign
