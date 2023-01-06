module Lib.Datagen.LootTable where

import Prelude

import Foreign.ReadWrite (class WriteForeign, writeForeign)
import Lib.Datagen.ResourceLocation (ResourceLocation)
import Lib.Json (unsafeFormatJson)
import Lib.Serializer (class IsDataType, class Serializable)

-- | The whole system is wayyy to complex to be worth modeling in full. Instead
-- | we'll just provide some templates.
data LootTable
  = SingleDrop ResourceLocation
  | SilkTouch ResourceLocation ResourceLocation
  | Slab ResourceLocation

-- | Template for single-drop tables with one entry
entry
  ∷ ∀ a
  . a
  → { type ∷ String
    , pools ∷ Array { bonus_rolls ∷ Number, rolls ∷ Int, entries ∷ Array a }
    }
entry a =
  { type: "minecraft:block"
  , pools:
      [ { bonus_rolls: 0.0
        , entries: [ a ]
        , rolls: 1
        }
      ]
  }

survivesExplosion ∷ Array { condition ∷ String }
survivesExplosion = [ { condition: "minecraft:survives_explosion" } ]

silkTouch
  ∷ Array
      { condition ∷ String
      , predicate ∷
          { enchantments ∷
              Array { enchantment ∷ String, levels ∷ { min ∷ Int } }
          }
      }
silkTouch =
  [ { condition: "minecraft:match_tool"
    , predicate:
        { enchantments:
            [ { enchantment: "minecraft:silk_touch", levels: { min: 1 } } ]
        }
    }
  ]

instance IsDataType LootTable where
  getFileExtension _ = "json"

instance WriteForeign LootTable where
  writeForeign (SingleDrop drop) = writeForeign $ entry
    { type: "minecraft:item"
    , name: drop
    , conditions: survivesExplosion
    }
  writeForeign (SilkTouch normal silked) = writeForeign $ entry
    { type: "minecraft:alternatives"
    , children:
        [ writeForeign
            { type: "minecraft:item"
            , name: silked
            , conditions: silkTouch
            }
        , writeForeign
            { type: "minecraft:item"
            , name: normal
            , conditions: survivesExplosion
            }
        ]
    }
  writeForeign (Slab slabBlock) = writeForeign $ entry
    { type: "minecraft:item"
    , name: slabBlock
    , functions:
        [ writeForeign
            { function: "minecraft:set_count"
            , conditions:
                [ { block: slabBlock
                  , condition: "minecraft:block_state_property"
                  , properties: { type: "double" }
                  }
                ]
            , count: 2
            , add: false
            }
        , writeForeign { function: "minecraft:explosion_decay" }
        ]
    }

instance Serializable LootTable where
  serialize lootTable = unsafeFormatJson (writeForeign lootTable)
