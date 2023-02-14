module Lib.Datagen.Model where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign.Object (Object, insert, singleton)
import Foreign.Object as Object
import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Lib.Json (unsafeFormatJson)
import Lib.ResourceLocation (ResourceLocation)
import Lib.Serializer (class IsDataType, class Serializable)

data Texture = Texture ResourceLocation | Reference String

instance WriteForeign Texture where
  writeForeign (Texture rl) = writeForeign rl
  writeForeign (Reference str) = writeForeign ("#" <> str)

-- | A model that specifies a parent and an object of textures
newtype Model = Model
  { parent ∷ ResourceLocation
  , textures ∷ Maybe (Object Texture)
  }

derive instance Newtype Model _
derive newtype instance WriteForeign Model

instance IsDataType Model where
  getFileExtension _ = "json"

instance Serializable Model where
  serialize model = unsafeFormatJson (writeForeign model)

itemModel ∷ ResourceLocation → Model
itemModel parent = Model { parent, textures: Nothing }

all ∷ ResourceLocation → Texture → Model
all parent texture = Model { parent, textures: Just $ singleton "all" texture }

pillar ∷ ResourceLocation → { side ∷ Texture, end ∷ Texture } → Model
pillar parent { side, end } = Model
  { parent
  , textures: Object.empty
      # insert "side" side
      # insert "end" end
      # Just
  }

directionalPillar
  ∷ ResourceLocation
  → { side ∷ Texture, top ∷ Texture, bottom ∷ Texture }
  → Model
directionalPillar parent { side, top, bottom } = Model
  { parent
  , textures: Object.empty
      # insert "side" side
      # insert "top" top
      # insert "bottom" bottom
      # Just
  }

hollowPillar
  ∷ ResourceLocation
  → { end ∷ Texture, inside ∷ Texture, side ∷ Texture }
  → Model
hollowPillar parent { side, inside, end } = Model
  { parent
  , textures: Object.empty
      # insert "side" side
      # insert "inside" inside
      # insert "end" end
      # Just
  }
