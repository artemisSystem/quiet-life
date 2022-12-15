module Lib.Datagen.Model where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign.Object (Object, empty, insert, singleton)
import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Lib.Json (unsafeFormatJson)
import Lib.Serializer (class IsDataType, class Serializable)

-- | A model that specifies a parent and an object of textures
newtype Model = Model
  { parent ∷ String
  , textures ∷ Maybe (Object String)
  }

derive instance Newtype Model _
derive newtype instance WriteForeign Model

instance IsDataType Model where
  getFileExtension _ = "json"

instance Serializable Model where
  serialize model = unsafeFormatJson (writeForeign model)

itemModel ∷ String → Model
itemModel parent = Model { parent, textures: Nothing }

all ∷ String → String → Model
all parent texture = Model { parent, textures: Just $ singleton "all" texture }

pillar ∷ String → { side ∷ String, end ∷ String } → Model
pillar parent { side, end } = Model
  { parent
  , textures: empty
      # insert "side" side
      # insert "end" end
      # Just
  }

hollowPillar ∷ String → { end ∷ String, inside ∷ String, side ∷ String } → Model
hollowPillar parent { side, inside, end } = Model
  { parent
  , textures: empty
      # insert "side" side
      # insert "inside" inside
      # insert "end" end
      # Just
  }
