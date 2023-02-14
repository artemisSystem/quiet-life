module Lib.BlockBuilder.Types where

import Lib.Datagen.Blockstate (Blockstate)
import Lib.Datagen.Model (Model)
import Lib.OnlyOne (UniqueRLMap)
import Lib.ResourceLocation (ResourceLocation)
import Run.Writer (Writer)
import Type.Proxy (Proxy(..))

data BlockWithType
  = Block ResourceLocation
  | Slab { base ∷ ResourceLocation, slab ∷ ResourceLocation }
  | VerticalSlab { base ∷ ResourceLocation, slab ∷ ResourceLocation }
  | Stairs { base ∷ ResourceLocation, slab ∷ ResourceLocation }
  | Wall { base ∷ ResourceLocation, slab ∷ ResourceLocation }

data BlockAddition
  = SlabAddition (ResourceLocation → ResourceLocation)
  | VerticalSlabAddition (ResourceLocation → ResourceLocation)
  | StairsAddition (ResourceLocation → ResourceLocation)
  | WallAddition (ResourceLocation → ResourceLocation)

type ModelsAndBlockstates r =
  ( blockstates ∷ Writer (UniqueRLMap Blockstate)
  , models ∷ Writer (UniqueRLMap Model)
  | r
  )

_blockstates ∷ Proxy "blockstates"
_blockstates = Proxy

_models ∷ Proxy "models"
_models = Proxy