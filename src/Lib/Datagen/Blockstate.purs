module Lib.Datagen.Blockstate where

import Data.Maybe (Maybe)
import Foreign.Object (Object)
import Foreign.ReadWrite (class WriteForeign, Default(..), default, undefined, writeForeign)
import Lib.Json (unsafeFormatJson)
import Lib.ResourceLocation (ResourceLocation)
import Lib.Serializer (class IsDataType, class Serializable)
import Prim.Boolean (False)

data Rotation = R0 | R90 | R180 | R270

instance WriteForeign Rotation where
  -- 0 is the default and can be omitted for brevity in places where a rotation
  -- is expected.
  writeForeign R0 = undefined
  writeForeign R90 = writeForeign 90
  writeForeign R180 = writeForeign 180
  writeForeign R270 = writeForeign 270

allRotations ∷ Array Rotation
allRotations = [ R0, R90, R180, R270 ]

nextRotation ∷ Rotation → Rotation
nextRotation R0 = R90
nextRotation R90 = R180
nextRotation R180 = R270
nextRotation R270 = R0

prevRotation ∷ Rotation → Rotation
prevRotation R0 = R270
prevRotation R90 = R0
prevRotation R180 = R90
prevRotation R270 = R180

type SingleVariant r =
  { model ∷ ResourceLocation
  , x ∷ Rotation
  , y ∷ Rotation
  , uvlock ∷ Default False Boolean
  | r
  }

data VariantModels
  = SingleVariant (SingleVariant ())
  | MultiVariant (Array (SingleVariant (weight ∷ Default 1 Int)))

singleVariant ∷ ResourceLocation → VariantModels
singleVariant model = rotatedVariant model R0 R0

rotatedVariant ∷ ResourceLocation → Rotation → Rotation → VariantModels
rotatedVariant model x y = SingleVariant
  { model, x, y, uvlock: default }

rotatedVariantUvLock ∷ ResourceLocation → Rotation → Rotation → VariantModels
rotatedVariantUvLock model x y = SingleVariant
  { model, x, y, uvlock: Default true }

aMultiVariant ∷ ResourceLocation → (SingleVariant (weight ∷ Default 1 Int))
aMultiVariant model = rotatedMultiVariant model R0 R0

rotatedMultiVariant
  ∷ ResourceLocation
  → Rotation
  → Rotation
  → (SingleVariant (weight ∷ Default 1 Int))
rotatedMultiVariant model x y =
  { model, x, y, uvlock: default, weight: default }

instance WriteForeign VariantModels where
  writeForeign (SingleVariant record) = writeForeign record
  writeForeign (MultiVariant records) = writeForeign records

type VariantBlockstate = Object VariantModels

data MultipartCase
  = ORCase { "OR" ∷ Array (Object String) }
  | StateCase (Object String)

instance WriteForeign MultipartCase where
  writeForeign (ORCase obj) = writeForeign obj
  writeForeign (StateCase obj) = writeForeign obj

type MultipartBlockstate = Array
  { apply ∷ VariantModels
  , when ∷ Maybe MultipartCase
  }

data Blockstate
  = VariantBlockstate VariantBlockstate
  | MultipartBlockstate MultipartBlockstate

instance IsDataType Blockstate where
  getFileExtension _ = "json"

instance WriteForeign Blockstate where
  writeForeign (VariantBlockstate variants) = writeForeign { variants }
  writeForeign (MultipartBlockstate multipart) = writeForeign { multipart }

instance Serializable Blockstate where
  serialize blockstate = unsafeFormatJson (writeForeign blockstate)
