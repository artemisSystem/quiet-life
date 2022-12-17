module Lib.Datagen.PackMcMeta where

import Prelude

import Data.Map (SemigroupMap)
import Data.Map as Map
import Data.Semigroup.First (First)
import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Lib.Json (unsafeFormatJson)
import Lib.Serializer (class IsDataType, class Serializable)
import Lib.Util (toSMap)

newtype PackMcMeta = PackMcMeta
  { pack_format ∷ Int
  , description ∷ String
  }

instance IsDataType PackMcMeta where
  getFileExtension _ = "mcmeta"

instance WriteForeign PackMcMeta where
  writeForeign (PackMcMeta pack) = writeForeign { pack }

instance Serializable PackMcMeta where
  serialize meta = unsafeFormatJson (writeForeign meta)

wrapMcMeta ∷ PackMcMeta → SemigroupMap String (First PackMcMeta)
wrapMcMeta meta = toSMap (Map.singleton "pack" $ meta)

-- | 1.19.2 pack_format
resourceMcmeta ∷ String → SemigroupMap String (First PackMcMeta)
resourceMcmeta description = wrapMcMeta $ PackMcMeta
  { pack_format: 9, description }

-- | 1.19.2 pack_format
dataMcmeta ∷ String → SemigroupMap String (First PackMcMeta)
dataMcmeta description = wrapMcMeta $ PackMcMeta
  { pack_format: 10, description }
