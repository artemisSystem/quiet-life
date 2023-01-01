module Lib.Datagen.PackMcMeta where

import Prelude

import Data.Map as Map
import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Lib.Json (unsafeFormatJson)
import Lib.OnlyOne (UniqueStrMap, toUMap)
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

-- TODO: these don't really need to be in maps. Could use OnlyOne directly but
-- requires an extra write data function i think
wrapMcMeta ∷ PackMcMeta → UniqueStrMap PackMcMeta
wrapMcMeta mcmeta = toUMap (Map.singleton "pack" $ mcmeta)

-- | 1.19.2 resource pack_format
resourceMcmeta ∷ String → UniqueStrMap PackMcMeta
resourceMcmeta description = wrapMcMeta $ PackMcMeta
  { pack_format: 9, description }

-- | 1.19.2 data pack_format
dataMcmeta ∷ String → UniqueStrMap PackMcMeta
dataMcmeta description = wrapMcMeta $ PackMcMeta
  { pack_format: 10, description }
