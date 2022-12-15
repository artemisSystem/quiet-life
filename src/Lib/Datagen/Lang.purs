module Lib.Datagen.Lang where

import Data.Map (Map)
import Foreign.Object as Object
import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Lib.Json (unsafeFormatJson)
import Lib.Serializer (class IsDataType, class Serializable)

newtype Lang = Lang (Map String String)

instance IsDataType Lang where
  getFileExtension _ = "json"

instance WriteForeign Lang where
  writeForeign (Lang langMap) = writeForeign
    (Object.fromFoldableWithIndex langMap)

instance Serializable Lang where
  serialize lang = unsafeFormatJson (writeForeign lang)
