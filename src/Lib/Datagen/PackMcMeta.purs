module Lib.Datagen.PackMcMeta where

import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Lib.Json (unsafeFormatJson)
import Lib.Serializer (class IsDataType, class Serializable)

newtype PackMcMeta = PackMcMeta
  { pack_format ∷ Int
  , description ∷ String
  }

instance IsDataType PackMcMeta where
  getFileExtension _ = "mcmeta"

instance WriteForeign PackMcMeta where
  writeForeign (PackMcMeta pack) = writeForeign { pack }

instance Serializable PackMcMeta where
  serialize mcmeta = unsafeFormatJson (writeForeign mcmeta)

-- | 1.19.2 resource pack_format
resourceMcmeta ∷ String → PackMcMeta
resourceMcmeta description = PackMcMeta
  { pack_format: 9, description }

-- | 1.19.2 data pack_format
dataMcmeta ∷ String → PackMcMeta
dataMcmeta description = PackMcMeta
  { pack_format: 10, description }
